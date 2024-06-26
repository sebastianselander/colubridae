{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Tc where

import Control.Lens.Getter (use, uses, view, views)
import Control.Lens.Setter (locally, modifying)
import Control.Lens.TH
import Control.Monad.Validate (MonadValidate, ValidateT, runValidateT)
import Control.Monad.Writer (Writer, runWriter)
import Data.Data (Data)
import Data.Generics.Schemes (listify)
import Data.Map.Strict qualified as Map
import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Typechecker.Subst
import Frontend.Typechecker.Types
import Relude hiding (Any, intercalate)
import Relude.Unsafe (fromJust)
import Frontend.Types
import Utils (chain, listify')
import Names (Ident, getOriginalName, Names)

data Env = Env
    { _variables :: Map Ident (TypeTc, SourceInfo)
    , _subst :: Subst
    }
    deriving (Show)

data Ctx = Ctx
    { _functions :: Map Ident (TypeTc, SourceInfo)
    , _returnType :: TypeTc
    , _currentFun :: DefRn
    , _names :: Names
    }
    deriving (Show)

$(makeLenses ''Env)
$(makeLenses ''Ctx)

newtype TcM a = Tc {runTc :: StateT Env (ReaderT Ctx (ValidateT [TcError] (Writer [TcWarning]))) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadValidate [TcError], MonadState Env)

run :: Ctx -> Env -> TcM a -> (Either [TcError] a, [TcWarning])
run ctx env = runWriter . runValidateT . flip runReaderT ctx . flip evalStateT env . runTc

getDefs :: (Data a) => a -> [(Ident, (TypeTc, SourceInfo))]
getDefs = listify' f
  where
    f :: DefRn -> Maybe (Ident, (TypeTc, SourceInfo))
    f (Fn info name args returnType _) =
        let funTy = TyFunX NoExtField (fmap argTy args) (typeOf returnType)
         in Just (name, (funTy, info))
    argTy :: ArgRn -> TypeTc
    argTy (ArgX (_, mut) _ ty) = case mut of
        Mutable -> Mut $ typeOf ty
        Immutable -> typeOf ty

tc :: Names -> ProgramRn -> (Either [TcError] ProgramTc, [TcWarning])
tc names (ProgramX NoExtField defs) = case first partitionEithers $ unzip $ fmap (tcDefs names funTable) defs of
    (([], defs), warnings) -> (Right $ ProgramX NoExtField defs, mconcat warnings)
    ((errs, _), warnings) -> (Left $ mconcat errs, mconcat warnings)
  where
    funTable = Map.fromList $ getDefs defs

tcDefs :: Names -> Map Ident (TypeTc, SourceInfo) -> DefRn -> (Either [TcError] DefTc, [TcWarning])
tcDefs names funTable fun@(Fn _ _ args rt _) =
    let varTable =
            foldr
                ( uncurry Map.insert
                    . ( \(ArgX (info, mut) name ty) ->
                            ( name
                            ,
                                ( case mut of
                                    Mutable -> Mut (typeOf ty)
                                    Immutable -> typeOf ty
                                , info
                                )
                            )
                      )
                )
                mempty
                args
        ctx = Ctx funTable (typeOf rt) fun names
        env = Env varTable nullSubst
     in run ctx env $ go fun
  where
    go (Fn _ name args rt block) =
        locally currentFun (const fun) $ do
            args <- mapM infArg args

            let retTy = typeOf rt
            block <- locally returnType (const retTy) $ case block of
                BlockX info stmts (Just expr) -> do
                    stmts <- mapM infStmt stmts
                    expr <- tcExpr retTy expr
                    pure $ BlockX (info, retTy) stmts (Just expr)
                BlockX info stmts Nothing -> do
                    stmts <- mapM infStmt stmts
                    pure $ BlockX (info, Any) stmts Nothing
            Fn NoExtField name args retTy <$> applySt block

infBlock :: BlockRn -> TcM BlockTc
infBlock (BlockX info statements tailExpression) = do
    stmts <- mapM infStmt statements
    expr <- mapM infExpr tailExpression
    pure $ BlockX (info, maybe Unit typeOf expr) stmts expr

tcBlock :: TypeTc -> BlockX Rn -> TcM BlockTc
tcBlock expectedTy (BlockX info statements tailExpression) = do
    stmts <- mapM infStmt statements
    expr <- mapM (tcExpr expectedTy) tailExpression
    pure $ BlockX (info, maybe Unit typeOf expr) stmts expr

infArg :: ArgRn -> TcM ArgTc
infArg (ArgX (_, _) name ty) = pure $ ArgX NoExtField name (typeOf ty)

infStmt :: StmtRn -> TcM StmtTc
infStmt (SExprX NoExtField expr) = SExprX NoExtField <$> infExpr expr

breakExpr :: ExprTc -> Bool
breakExpr = \case
    BreakX {} -> True
    _ -> False

infExpr :: ExprRn -> TcM ExprTc
infExpr currentExpr = case currentExpr of
    LitX info lit ->
        let (ty, b) = infLit lit
         in pure $ LitX (info, ty) b
    VarX (info, bind) name -> do
        (ty, _declaredAtInfo) <- case bind of
            Free -> lookupVar name
            Bound -> lookupVar name
            Lambda -> lookupVar name
            Toplevel -> (\(ty, info) -> (ty, info)) <$> lookupFun name
        pure $ VarX (info, ty, bind) name
    PrefixX info op expr -> do
        expr <- tcExpr Bool expr
        pure $ PrefixX (info, Bool) op expr
    BinOpX info l op r -> do
        let types = operatorTypes op
        l <- infExpr l
        let ty = typeOf l
        r <- tcExpr ty r
        ty <- pure $ operatorReturnType ty op
        if typeOf r `elem` types
            then pure $ BinOpX (info, ty) l op r
            else do
                ty <- Any <$ invalidOperatorType info currentExpr op (typeOf r)
                pure $ BinOpX (info, ty) l op r
    AppX info l r -> do
        l <- infExpr l
        case typeOf l of
            TyFunX NoExtField argTys retTy -> do
                let argTysLength = length argTys
                let rLength = length r
                if
                    | argTysLength < rLength -> do
                        retTy <- Any <$ tooManyArguments info currentExpr argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX (info, retTy) l r
                    | argTysLength > rLength -> do
                        retTy <- Any <$ partiallyAppliedFunction info currentExpr argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX (info, retTy) l r
                    | otherwise -> do
                        -- NOTE: argTys and r are of equal length
                        r <- zipWithM tcExpr argTys r
                        pure $ AppX (info, retTy) l r
            ty -> do
                retTy <- Any <$ applyNonFunction info currentExpr ty
                r <- mapM infExpr r
                pure $ AppX (info, retTy) l r
    LetX (info, mut, mbty) name expr -> do
        expr <- maybe (infExpr expr) ((`tcExpr` expr) . typeOf) mbty
        let ty = case mut of
                Mutable -> Mut (typeOf expr)
                Immutable -> typeOf expr
        insertVar name ty info
        applySt $ LetX (StmtType Unit ty info) name expr
    AssX (info, bind) name op expr -> do
        (ty, info) <- case bind of
            Toplevel -> assignNonVariable @TcM info currentExpr <$> views names (getOriginalName name) >> pure (Any, info)
            _ -> lookupVar name
        ty <- case ty of
            Mut ty -> pure ty
            _ -> do
                name <- views names (getOriginalName name)
                Any <$ immutableVariable @TcM info currentExpr name
        expr <- tcExpr ty expr
        unify info currentExpr ty expr
        ty <- applySt ty
        case op of
            AddAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot info currentExpr [Int, Double] ty)
            SubAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot info currentExpr [Int, Double] ty)
            MulAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot info currentExpr [Int, Double] ty)
            DivAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot info currentExpr [Int, Double] ty)
            ModAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot info currentExpr [Int, Double] ty)
            Assign -> pure ()
        pure $ AssX (StmtType Unit ty info, bind) name op expr
    RetX info mbExpr -> do
        returnType <- view returnType
        case mbExpr of
            Nothing -> do
                unless (returnType == Unit) (void $ emptyReturnNonUnit info currentExpr returnType)
                pure $ RetX (info, Unit) Nothing
            Just expr -> do
                expr <- tcExpr returnType expr
                pure $ RetX (info, returnType) (Just expr)
    EBlockX NoExtField block -> EBlockX NoExtField <$> infBlock block
    BreakX info expr -> do
        expr <- mapM infExpr expr
        pure $ BreakX (info, maybe Unit typeOf expr) expr
    IfX info condition true false -> do
        condition <- tcExpr Bool condition
        true <- infBlock true
        let ty = typeOf true
        false <- mapM (tcBlock ty) false
        pure $ IfX (info, ty) condition true false
    WhileX info expr block -> do
        let blockType = Unit
        expr <- tcExpr Bool expr
        block <- tcBlock blockType block
        case block of
            BlockX _ stmt tail -> do
                let breakExprs = listify breakExpr stmt
                maybe (pure ()) (unify info currentExpr blockType) tail
                mapM_ (\expr -> unify (hasInfo expr) currentExpr Unit expr) breakExprs
        pure $ WhileX (info, Unit) expr block
    ExprX (LoopX info block) -> do
        block@(BlockX _ stmts tail) <- infBlock block
        ty <- case listify breakExpr (maybe stmts (\x -> SExprX NoExtField x : stmts) tail) of
            [] -> pure Any
            (x : xs) -> do
                sequence_
                    $ chain
                        (\expr1 expr2 -> unify (hasInfo expr2) currentExpr (typeOf expr1) expr2)
                        x
                        xs
                typeOf <$> applySt x
        applySt $ ExprX $ LoopX (info, ty) block

tcExpr :: TypeTc -> ExprRn -> TcM ExprTc
tcExpr expectedTy currentExpr = case currentExpr of
    LitX info lit -> do
        let (ty, lit') = infLit lit
        let literal = LitX (info, ty) lit'
        void $ unify info currentExpr expectedTy literal
        applySt literal
    VarX (info, bind) name -> do
        (ty, _declaredAtInfo) <- case bind of
            Free -> lookupVar name
            Bound -> lookupVar name
            Lambda -> lookupVar name
            Toplevel -> (\(ty, info) -> (ty, info)) <$> lookupFun name
        let expr = VarX (info, ty, bind) name
        unify info currentExpr expectedTy expr
        applySt expr
    PrefixX info op expr -> do
        case op of
            Not -> do
                expr <- tcExpr Bool expr
                let expr' = PrefixX (info, Bool) op expr
                unify info currentExpr expectedTy expr'
                pure expr'
            Neg -> do
                expr <- infExpr expr
                let ty = typeOf expr
                unless (ty `elem` [Int, Double]) (tyExpectedGot info currentExpr [Int, Double] ty)
                pure $ PrefixX (info, ty) op expr
    BinOpX info l op r -> do
        l <- infExpr l
        r <- tcExpr (typeOf l) r
        let ty = operatorReturnType (typeOf r) op
        let expr = BinOpX (info, ty) l op r
        unify info currentExpr expectedTy expr
        applySt expr
    AppX info fun args -> do
        expr <- infExpr (AppX info fun args)
        unify info currentExpr expectedTy expr
        applySt expr
    LetX (info, mut, mbty) name expr -> do
        expr <- maybe (infExpr expr) ((`tcExpr` expr) . typeOf) mbty
        let ty = case mut of
                Mutable -> Mut (typeOf expr)
                Immutable -> typeOf expr
        unify' info currentExpr expectedTy Unit
        insertVar name ty info
        applySt $ LetX (StmtType Unit ty info) name expr
    AssX (info, bind) name op expr -> do
        expr <- infExpr (AssX (info, bind) name op expr)
        unify info currentExpr expectedTy expr
        applySt expr
    RetX info expr -> do
        returnType <- view returnType
        case expr of
            Nothing -> do
                unify' info currentExpr returnType Unit
                pure $ RetX (info, Any) Nothing
            Just expr -> do
                expr <- tcExpr returnType expr
                applySt $ RetX (info, Any) (Just expr)
    EBlockX NoExtField block -> do
        block <- tcBlock expectedTy block
        pure $ EBlockX NoExtField block
    BreakX info expr -> do
        expr <- mapM infExpr expr
        pure $ BreakX (info, Any) expr
    IfX info cond true false -> do
        cond <- tcExpr Bool cond
        true <- infBlock true
        let ty = typeOf true
        false <- mapM (tcBlock ty) false
        pure $ IfX (info, ty) cond true false
    while@(WhileX info _ _) -> do
        while <- infExpr while
        unify info currentExpr expectedTy while
        pure while
    loop@(ExprX (LoopX info _)) -> do
        loop <- infExpr loop
        unify info currentExpr expectedTy loop
        pure loop

hasInfo :: ExprTc -> SourceInfo
hasInfo = \case
    LitX info _ -> fst info
    VarX (info, _, _) _ -> info
    PrefixX info _ _ -> fst info
    BinOpX info _ _ _ -> fst info
    AppX info _ _ -> fst info
    LetX info _ _ -> view stmtInfo info
    AssX (info, _) _ _ _ -> view stmtInfo info
    RetX info _ -> fst info
    EBlockX _ (BlockX info _ _) -> fst info
    BreakX info _ -> fst info
    IfX info _ _ _ -> fst info
    WhileX info _ _ -> fst info
    ExprX (LoopX info _) -> fst info

operatorReturnType :: TypeTc -> BinOp -> TypeTc
operatorReturnType inputTy = \case
    Mul -> inputTy
    Div -> inputTy
    Add -> inputTy
    Sub -> inputTy
    Mod -> inputTy
    Or -> Bool
    And -> Bool
    Lt -> Bool
    Gt -> Bool
    Lte -> Bool
    Gte -> Bool
    Eq -> Bool
    Neq -> Bool

operatorTypes :: BinOp -> [TypeTc]
operatorTypes = \case
    Mul -> [Int, Double]
    Div -> [Int, Double]
    Add -> [Int, Double]
    Sub -> [Int, Double]
    Mod -> [Int, Double]
    Or -> [Bool]
    And -> [Bool]
    Lt -> [Int, Double, Bool, String]
    Gt -> [Int, Double, Bool, String]
    Lte -> [Int, Double, Bool, String]
    Gte -> [Int, Double, Bool, String]
    Eq -> [Int, Double, Bool, String]
    Neq -> [Int, Double, Bool, String]

infLit :: LitRn -> (TypeTc, LitTc)
infLit = \case
    IntLitX info n -> (Int, IntLitX info n)
    DoubleLitX info n -> (Double, DoubleLitX info n)
    StringLitX info s -> (String, StringLitX info s)
    CharLitX info c -> (Char, CharLitX info c)
    BoolLitX info b -> (Bool, BoolLitX info b)
    UnitLitX info -> (Unit, UnitLitX info)

insertVar :: (MonadState Env m) => Ident -> TypeTc -> SourceInfo -> m ()
insertVar name ty info = modifying variables (Map.insert name (ty, info))

lookupVar :: (MonadState Env m) => Ident -> m (TypeTc, SourceInfo)
lookupVar name = uses variables (fromJust . Map.lookup name)

lookupVarTy :: (MonadState Env m) => Ident -> m TypeTc
lookupVarTy = fmap fst . lookupVar

lookupFun :: (MonadReader Ctx m) => Ident -> m (TypeTc, SourceInfo)
lookupFun name = views functions (fromJust . Map.lookup name)

putSubst :: (MonadState Env m) => Subst -> m ()
putSubst sub = modifying subst (`compose` sub)

applySt :: (MonadState Env m, Substitution a) => a -> m a
applySt a = do
    sub <- use subst
    pure $ apply sub a

class TypeOf a where
    typeOf :: a -> TypeTc

instance TypeOf StmtTc where
    typeOf = \case
        SExprX _ expr -> typeOf expr

instance TypeOf ExprTc where
    typeOf = \case
        LitX ty _ -> snd ty
        VarX (_,ty,_) _ -> ty
        PrefixX ty _ _ -> snd ty
        BinOpX ty _ _ _ -> snd ty
        AppX ty _ _ -> snd ty
        LetX rec _ _ -> view stmtType rec
        AssX (rec, _) _ _ _ -> view stmtType rec
        RetX ty _ -> snd ty
        EBlockX _ block -> typeOf block
        BreakX ty _ -> snd ty
        IfX ty _ _ _ -> snd ty
        WhileX ty _ _ -> snd ty
        ExprX (LoopX ty _) -> snd ty

instance TypeOf BlockTc where
    typeOf (BlockX ty _ _) = snd ty

instance TypeOf TypeRn where
    typeOf = \case
        TyLitX a b -> TyLitX a b
        TyFunX a b c -> TyFunX a (fmap typeOf b) (typeOf c)

unify ::
    (MonadState Env m, MonadValidate [TcError] m, TypeOf a) =>
    SourceInfo ->
    ExprRn ->
    TypeTc ->
    a ->
    m ()
unify info currentExpr ty1 a = unify' info currentExpr ty1 (typeOf a)

-- | Unify two types. The first argument type *must* the expected one!
unify' ::
    (MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo ->
    ExprRn ->
    TypeTc ->
    TypeTc ->
    m ()
unify' info currentExpr ty1 ty2 = case (ty1, ty2) of
    (Any, _) -> pure ()
    (_, Any) -> pure ()
    (TyLitX _ lit1, TyLitX _ lit2)
        | lit1 == lit2 -> pure ()
        | otherwise -> void $ tyExpectedGot info currentExpr [ty1] ty2
    (TyFunX _ l1 r1, TyFunX _ l2 r2) -> do
        unless (length l1 == length l2) (void $ tyExpectedGot info currentExpr [ty1] ty2)
        zipWithM_ (unify' info currentExpr) l1 l2
        r1 <- applySt r1
        r2 <- applySt r2
        unify' info currentExpr r1 r2
    (Mut ty1, Mut ty2) -> unify' info currentExpr ty1 ty2
    (ty1, Mut ty2) -> unify' info currentExpr ty1 ty2
    (Mut ty1, ty2) -> onlyImmutable info currentExpr (MutableX ty1) ty2
    (ty1, ty2) -> void $ tyExpectedGot info currentExpr [ty1] ty2
