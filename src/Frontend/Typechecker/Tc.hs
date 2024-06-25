{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Tc where

import Control.Lens.Getter (use, uses, view, views)
import Control.Lens.Operators ((+=))
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
import Relude hiding (intercalate)
import Relude.Unsafe (fromJust)
import Types
import Utils (chain, listify')

-- TODO: Wrap each checking result in maybe and explicitly decide to continue or not to gather all errors
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
tcDefs (Fn info name args rt block) = do
    let arguments = fmap (\(ArgX (info, mut) name ty) -> (name, (case mut of Mutable -> Mut (typeOf ty); Immutable -> typeOf ty, info))) args
    args <- mapM tcArg args
    modifying variables (\vars -> foldr (uncurry Map.insert) vars arguments)
    block <- locally returnType (const (typeOf rt)) $ infBlock block
    case block of
        BlockX _ _ (Just expr) -> unify info (typeOf rt) expr
        BlockX _ _ Nothing -> pure ()
    Fn NoExtField name args (typeOf rt) <$> applySt block

-- TODO: Break statments must have the same type as the block in loops
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

tcArg :: ArgRn -> TcM ArgTc
tcArg (ArgX (_, _) name ty) = pure $ ArgX NoExtField name (typeOf ty)

infStmt :: StmtRn -> TcM StmtTc
infStmt stmt = view returnType >>= flip go stmt
  where
    go returnType = \case
        RetX info mbExpr -> do
            case mbExpr of
                Nothing -> do
                    unless (returnType == Unit) (void $ emptyReturnNonUnit info returnType)
                    pure $ RetX (info, Unit) Nothing
                Just expr -> do
                    expr <- tcExpr returnType expr
                    pure $ RetX (info, returnType) (Just expr)
        SBlockX NoExtField block -> SBlockX NoExtField <$> infBlock block
        BreakX info expr -> do
            expr <- mapM infExpr expr
            pure $ BreakX (info, maybe Unit typeOf expr) expr
        IfX info condition true false -> do
            condition <- tcExpr Bool condition
            true <- infBlock true
            false <- mapM infBlock false
            pure $ IfX (info, Unit) condition true false
        WhileX info expr block -> do
            expr <- tcExpr Bool expr
            block <- infBlock block
            case block of
                BlockX _ stmt Nothing -> mapM_ breakValueless stmt
                BlockX _ stmt (Just tail) -> do
                    mapM_ breakValueless stmt
                    unify info Unit tail
            pure $ WhileX (info, Unit) expr block
        SExprX NoExtField expr -> do
            SExprX NoExtField <$> infExpr expr
        StmtX (LoopX info block) -> do
            block@(BlockX _blockTy stmts tail) <- tcBlock Unit block
            maybe (pure ()) (unify info Unit) tail
            ty <- case listify' breakTypes stmts of
                [] -> pure Unit
                (x : xs) -> do
                    sequence_ $ chain (unify' info) x xs
                    applySt x
            applySt $ StmtX $ LoopX (info, ty) block

breakTypes :: StmtTc -> Maybe TypeTc
breakTypes = \case
    BreakX ty _ -> Just $ snd ty
    _ -> Nothing

breakValueless :: (MonadValidate [TcError] m) => StmtTc -> m ()
breakValueless = \case
    BreakX (info, _) (Just _) -> void $ nonEmptyBreak info
    _ -> pure ()

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
        meta <- freshMeta
        putSubst $ singleton meta ty
        pure $ VarX (info, ty) name
    BinOpX info l op r -> do
        let types = operatorTypes op
        l <- infExpr l
        let ty = typeOf l
        r <- tcExpr ty r
        ty <- pure $ operatorReturnType ty op
        if typeOf r `elem` types
            then pure $ BinOpX (info, ty) l op r
            else do
                ty <- Unsolvable <$ invalidOperatorType info op (typeOf r)
                pure $ BinOpX (info, ty) l op r
    AppX info l r -> do
        l <- infExpr l
        -- TODO: Simplify this logic
        -- TODO: Check for mutable argument stuff
        case typeOf l of
            TyFunX NoExtField argTys retTy -> do
                let argTysLength = length argTys
                let rLength = length r
                if
                    | argTysLength < rLength -> do
                        retTy <- Unsolvable <$ tooManyArguments info argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX (info, retTy) l r
                    | argTysLength > rLength -> do
                        retTy <- Unsolvable <$ partiallyAppliedFunction info argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX (info, retTy) l r
                    | otherwise -> do
                        -- NOTE: argTys and r are of equal length
                        r <- zipWithM tcExpr argTys r
                        pure $ AppX (info, retTy) l r
            ty -> do
                retTy <- Unsolvable <$ applyNonFunction info ty
                r <- mapM infExpr r
                pure $ AppX (info, retTy) l r
    EStmtX info stmt -> do
        stmt <- infStmt stmt
        case stmt of
            IfX _ cond true false -> do
                ty <- case false of
                    Nothing -> Unsolvable <$ missingElse info
                    Just false -> do
                        unify info (typeOf true) false
                        applySt (typeOf true)
                applySt $ EStmtX NoExtField $ IfX (info, ty) cond true false
            stmt -> pure $ EStmtX NoExtField stmt
    LetX (info, mut, mbty) name expr -> do
        ty <- case mbty of
            Nothing -> TypeX <$> freshMeta
            Just ty -> pure $ typeOf ty
        expr <- tcExpr ty expr
        unify info ty expr
        ty <- applySt ty
        let ty' = case mut of
                Mutable -> Mut ty
                Immutable -> ty
        insertVar name ty' info
        applySt $ LetX (StmtType Unit ty info) name expr
    AssX (info, bind) name op expr -> do
        (ty, info) <- case bind of
            Toplevel -> assignNonVariable info name >> pure (Unsolvable, info)
            _ -> lookupVar name
        case ty of
            Mut _ -> pure ()
            _ -> void $ immutableVariable info name
        expr <- tcExpr ty expr
        unify info ty expr
        ty <- applySt ty
        case op of
            AddAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            SubAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            MulAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            DivAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            ModAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            Assign -> pure ()
        pure $ AssX (StmtType Unit ty info) name op expr

tcExpr :: TypeTc -> ExprRn -> TcM ExprTc
tcExpr expectedTy = \case
    LitX info lit -> do
        let (ty, lit') = infLit lit
        let literal = LitX (info, ty) lit'
        void $ unify info expectedTy literal
        applySt literal
    VarX (info, bind) name -> do
        expr <- infExpr (VarX (info, bind) name)
        unify info expectedTy expr
        applySt expr
    BinOpX info l op r -> do
        l <- infExpr l
        r <- tcExpr (typeOf l) r
        let ty = operatorReturnType (typeOf r) op
        let expr = BinOpX (info, ty) l op r
        unify info expectedTy expr
        applySt expr
    AppX info fun args -> do
        expr <- infExpr (AppX info fun args)
        unify info expectedTy expr
        applySt expr
    LetX (info, mut, mbty) name expr -> do
        expr <- infExpr (LetX (info, mut, mbty) name expr)
        unify info expectedTy expr
        applySt expr
    AssX (info, bind) name op expr -> do
        expr <- infExpr (AssX (info, bind) name op expr)
        unify info expectedTy expr
        applySt expr
    EStmtX info stmt -> do
        expr <- infExpr (EStmtX info stmt)
        unify info expectedTy expr
        applySt expr

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

freshMeta :: (MonadState Env m) => m MetaTy
freshMeta = do
    n <- use freshCounter
    freshCounter += 1
    pure $ MetaX n

putSubst :: (MonadState Env m) => Subst -> m ()
putSubst sub = modifying subst (`compose` sub)

applySt :: (MonadState Env m, Substitution a) => a -> m a
applySt a = do
    sub <- use subst
    pure $ apply sub a

class TypeOf a where
    typeOf :: a -> TypeTc

-- TODO: Remove types from statements and move to statement-expressions
instance TypeOf StmtTc where
    typeOf = \case
        RetX ty _ -> snd ty
        SBlockX _ block -> typeOf block
        BreakX ty _ -> snd ty
        IfX ty _ _ _ -> snd ty
        WhileX ty _ _ -> snd ty
        SExprX _ expr -> typeOf expr
        StmtX (LoopX ty _) -> snd ty

instance TypeOf ExprTc where
    typeOf = \case
        LitX ty _ -> snd ty
        VarX ty _ -> snd ty
        BinOpX ty _ _ _ -> snd ty
        AppX ty _ _ -> snd ty
        LetX rec _ _ -> view stmtType rec
        AssX rec _ _ _ -> view stmtType rec
        EStmtX _ stmt -> typeOf stmt

instance TypeOf BlockTc where
    typeOf (BlockX ty _ _) = snd ty

instance TypeOf TypeRn where
    typeOf = \case
        TyLitX a b -> TyLitX a b
        TyVarX a b -> TyVarX a b
        TyFunX a b c -> TyFunX a (fmap typeOf b) (typeOf c)

unify :: (MonadState Env m, MonadValidate [TcError] m, TypeOf a) => SourceInfo -> TypeTc -> a -> m ()
unify info ty1 a = unify' info ty1 (typeOf a)

-- | Unify two types. The first argument type *must* the expected one!
unify' :: (MonadState Env m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> TypeTc -> m ()
unify' info ty1 ty2 = case (ty1, ty2) of
    (Unsolvable, _) -> doneTcError
    (_, Unsolvable) -> doneTcError
    (Meta meta, ty2) -> putSubst $ singleton (MetaX meta) ty2
    (ty1, Meta meta) -> putSubst $ singleton (MetaX meta) ty1
    (TyLitX _ lit1, TyLitX _ lit2)
        | lit1 == lit2 -> pure ()
        | otherwise -> void $ tyExpectedGot info [ty1] ty2
    (TyVarX _ name1, TyVarX _ name2)
        | name1 == name2 -> pure ()
        | otherwise -> void $ tyExpectedGot info [ty1] ty2
    (TyFunX _ l1 r1, TyFunX _ l2 r2) -> do
        unless (length l1 == length l2) (void $ tyExpectedGot info [ty1] ty2)
        zipWithM_ (unify' info) l1 l2
        r1 <- applySt r1
        r2 <- applySt r2
        unify' info r1 r2
    (Mut ty1, Mut ty2) -> unify' info ty1 ty2
    (ty1, Mut ty2) -> unify' info ty1 ty2
    (Mut ty1, ty2) -> onlyImmutable info (MutableX ty1) ty2
    (ty1, ty2) -> void $ tyExpectedGot info [ty1] ty2
