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
import Data.Map.Strict qualified as Map
import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Typechecker.Subst
import Frontend.Typechecker.Types
import Relude hiding (Any, intercalate)
import Relude.Unsafe (fromJust)
import Types
import Utils (chain, listify')

-- TODO: Wrap each checking result in maybe and explicitly decide to continue
-- or not to gather all errors
data Env = Env {_variables :: Map Ident (TypeTc, SourceInfo), _freshCounter :: Int, _subst :: Subst}
    deriving (Show)

data Ctx = Ctx {_functions :: Map Ident (TypeTc, SourceInfo), _returnType :: TypeTc}
    deriving (Show)

$(makeLenses ''Env)
$(makeLenses ''Ctx)

newtype TcM a = Tc {runTc :: StateT Env (ReaderT Ctx (ValidateT [TcError] (Writer [TcWarning]))) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadValidate [TcError], MonadState Env)

tc :: ProgramRn -> (Either [TcError] ProgramTc, [TcWarning])
tc program =
    runWriter
        . runValidateT
        . flip runReaderT initCtx
        . flip evalStateT initEnv
        . runTc
        . tcProg
        $ program
  where
    initCtx = Ctx (Map.fromList $ getDefs program) Unit
    initEnv = Env mempty 0 nullSubst

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

tcProg :: ProgramRn -> TcM ProgramTc
tcProg (ProgramX NoExtField defs) = ProgramX NoExtField <$> mapM tcDefs defs

-- TODO: Add the function to callstack for more precise error reporting
tcDefs :: DefRn -> TcM (DefX Tc)
tcDefs (Fn _ name args rt block) = do
    let arguments =
            fmap
                ( \(ArgX (info, mut) name ty) ->
                    (name, (case mut of Mutable -> Mut (typeOf ty); Immutable -> typeOf ty, info))
                )
                args
    args <- mapM infArg args
    modifying variables (\vars -> foldr (uncurry Map.insert) vars arguments)
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

breakExpr :: ExprTc -> Maybe ExprTc
breakExpr = \case
    e@BreakX {} -> Just e
    _ -> Nothing

infExpr :: ExprRn -> TcM ExprTc
infExpr = \case
    LitX info lit ->
        let (ty, b) = infLit lit
         in pure $ LitX (info, ty) b
    VarX (info, bind) name -> do
        (ty, _declaredAtInfo) <- case bind of
            Free -> lookupVar name
            Bound -> lookupVar name
            Lambda -> lookupVar name
            Toplevel -> (\(ty, info) -> (ty, info)) <$> lookupFun name
        pure $ VarX (info, ty) name
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
                ty <- Any <$ invalidOperatorType info op (typeOf r)
                pure $ BinOpX (info, ty) l op r
    AppX info l r -> do
        l <- infExpr l
        case typeOf l of
            TyFunX NoExtField argTys retTy -> do
                let argTysLength = length argTys
                let rLength = length r
                if
                    | argTysLength < rLength -> do
                        retTy <- Any <$ tooManyArguments info argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX (info, retTy) l r
                    | argTysLength > rLength -> do
                        retTy <- Any <$ partiallyAppliedFunction info argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX (info, retTy) l r
                    | otherwise -> do
                        -- NOTE: argTys and r are of equal length
                        r <- zipWithM tcExpr argTys r
                        pure $ AppX (info, retTy) l r
            ty -> do
                retTy <- Any <$ applyNonFunction info ty
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
            Toplevel -> assignNonVariable info name >> pure (Any, info)
            _ -> lookupVar name
        case ty of
            Mut _ -> pure ()
            _ -> void $ immutableVariable info name
        expr <- tcExpr ty expr
        unify Nothing info ty expr
        ty <- applySt ty
        case op of
            AddAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot Nothing info [Int, Double] ty)
            SubAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot Nothing info [Int, Double] ty)
            MulAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot Nothing info [Int, Double] ty)
            DivAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot Nothing info [Int, Double] ty)
            ModAssign ->
                unless
                    (ty `elem` [Int, Double])
                    (void $ tyExpectedGot Nothing info [Int, Double] ty)
            Assign -> pure ()
        pure $ AssX (StmtType Unit ty info) name op expr
    RetX info mbExpr -> do
        returnType <- view returnType
        case mbExpr of
            Nothing -> do
                unless (returnType == Unit) (void $ emptyReturnNonUnit info returnType)
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
                let breakExprs = listify' breakExpr stmt
                maybe (pure ()) (unify Nothing info blockType) tail
                mapM_ (\expr -> unify Nothing (hasInfo expr) Unit expr) breakExprs
        pure $ WhileX (info, Unit) expr block
    ExprX (LoopX info block) -> do
        --BUG: Not all breaks are caught, check bad/ex1
        block@(BlockX _ stmts _) <- infBlock block
        ty <- case listify' breakExpr stmts of
            [] -> pure Any
            (x : xs) -> do
                sequence_
                    $ chain
                        (\expr1 expr2 -> unify Nothing (hasInfo expr2) (typeOf expr1) expr2)
                        x
                        xs
                typeOf <$> applySt x
        applySt $ ExprX $ LoopX (info, ty) block

tcExpr :: TypeTc -> ExprRn -> TcM ExprTc
tcExpr expectedTy = \case
    LitX info lit -> do
        let (ty, lit') = infLit lit
        let literal = LitX (info, ty) lit'
        void $ unify Nothing info expectedTy literal
        applySt literal
    VarX (info, bind) name -> do
        (ty, declaredAt) <- case bind of
            Free -> lookupVar name
            Bound -> lookupVar name
            Lambda -> lookupVar name
            Toplevel -> (\(ty, info) -> (ty, info)) <$> lookupFun name
        let expr = VarX (info, ty) name
        unify (Just declaredAt) info expectedTy expr
        applySt expr
    PrefixX info op expr -> do
        case op of
            Not -> do
                expr <- tcExpr Bool expr
                let expr' = PrefixX (info, Bool) op expr
                unify Nothing info expectedTy expr'
                pure expr'
            Neg -> do
                expr <- infExpr expr
                let ty = typeOf expr
                unless (ty `elem` [Int, Double]) (tyExpectedGot Nothing info [Int, Double] ty)
                pure $ PrefixX (info, ty) op expr
    BinOpX info l op r -> do
        l <- infExpr l
        r <- tcExpr (typeOf l) r
        let ty = operatorReturnType (typeOf r) op
        let expr = BinOpX (info, ty) l op r
        unify Nothing info expectedTy expr
        applySt expr
    AppX info fun args -> do
        expr <- infExpr (AppX info fun args)
        unify Nothing info expectedTy expr
        applySt expr
    LetX (info, mut, mbty) name expr -> do
        expr <- maybe (infExpr expr) ((`tcExpr` expr) . typeOf) mbty
        let ty = case mut of
                Mutable -> Mut (typeOf expr)
                Immutable -> typeOf expr
        unify' Nothing info expectedTy Unit
        insertVar name ty info
        applySt $ LetX (StmtType Unit ty info) name expr
    AssX (info, bind) name op expr -> do
        expr <- infExpr (AssX (info, bind) name op expr)
        unify Nothing info expectedTy expr
        applySt expr
    RetX info expr -> do
        returnType <- view returnType
        case expr of
            Nothing -> do
                unify' Nothing info returnType Unit
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
        unify Nothing info expectedTy while
        pure while
    loop@(ExprX (LoopX info _)) -> do
        --BUG: Not all breaks are caught, check bad/ex1
        loop <- infExpr loop
        unify Nothing info expectedTy loop
        pure loop

hasInfo :: ExprTc -> SourceInfo
hasInfo = \case
    LitX info _ -> fst info
    VarX info _ -> fst info
    PrefixX info _ _ -> fst info
    BinOpX info _ _ _ -> fst info
    AppX info _ _ -> fst info
    LetX info _ _ -> view stmtInfo info
    AssX info _ _ _ -> view stmtInfo info
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
        VarX ty _ -> snd ty
        PrefixX ty _ _ -> snd ty
        BinOpX ty _ _ _ -> snd ty
        AppX ty _ _ -> snd ty
        LetX rec _ _ -> view stmtType rec
        AssX rec _ _ _ -> view stmtType rec
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

unify
    :: (MonadState Env m, MonadValidate [TcError] m, TypeOf a)
    => Maybe SourceInfo
    -> SourceInfo
    -> TypeTc
    -> a
    -> m ()
unify declaredAt info ty1 a = unify' declaredAt info ty1 (typeOf a)

-- | Unify two types. The first argument type *must* the expected one!
unify'
    :: (MonadState Env m, MonadValidate [TcError] m)
    => Maybe SourceInfo
    -> SourceInfo
    -> TypeTc
    -> TypeTc
    -> m ()
unify' declaredAt info ty1 ty2 = case (ty1, ty2) of
    (Any, _) -> pure ()
    (_, Any) -> pure ()
    (TyLitX _ lit1, TyLitX _ lit2)
        | lit1 == lit2 -> pure ()
        | otherwise -> void $ tyExpectedGot declaredAt info [ty1] ty2
    (TyFunX _ l1 r1, TyFunX _ l2 r2) -> do
        unless (length l1 == length l2) (void $ tyExpectedGot declaredAt info [ty1] ty2)
        zipWithM_ (unify' declaredAt info) l1 l2
        r1 <- applySt r1
        r2 <- applySt r2
        unify' declaredAt info r1 r2
    (Mut ty1, Mut ty2) -> unify' declaredAt info ty1 ty2
    (ty1, Mut ty2) -> unify' declaredAt info ty1 ty2
    (Mut ty1, ty2) -> onlyImmutable info (MutableX ty1) ty2
    (ty1, ty2) -> void $ tyExpectedGot declaredAt info [ty1] ty2
