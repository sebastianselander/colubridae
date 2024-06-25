{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Tc where

import Control.Arrow (left)
import Control.Lens.Getter (use, uses, views, view)
import Control.Lens.Operators ((+=))
import Control.Lens.Setter (modifying, locally)
import Control.Lens.TH
import Control.Monad.Validate (MonadValidate, Validate, runValidate)
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Data.Text (intercalate)
import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Typechecker.Subst
import Frontend.Typechecker.Types
import Relude hiding (intercalate)
import Relude.Unsafe (fromJust)
import Types
import Utils (listify')

-- TODO: Wrap each checking result in maybe and explicitly decide to continue or not to gather all errors
data Env = Env {_variables :: Map Ident (TypeTc, Mutability), _freshCounter :: Int, _subst :: Subst}
    deriving (Show)

data Ctx = Ctx {_functions :: Map Ident TypeTc, _returnType :: TypeTc}
    deriving (Show)

$(makeLenses ''Env)
$(makeLenses ''Ctx)

newtype TcM a = Tc {runTc :: StateT Env (ReaderT Ctx (Validate [TcError])) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadValidate [TcError], MonadState Env)

tc :: ProgramRn -> Either Text ProgramTc
tc program =
    left (intercalate "\n\n" . fmap report)
        . runValidate
        . flip runReaderT initCtx
        . flip evalStateT initEnv
        . runTc
        . tcProg
        $ program
  where
    initCtx = Ctx (Map.fromList $ getDefs program) Unit
    initEnv = Env mempty 0 nullSubst

getDefs :: (Data a) => a -> [(Ident, TypeTc)]
getDefs = listify' f
  where
    f :: DefRn -> Maybe (Ident, TypeTc)
    f (Fn _ name args returnType _) =
        let funTy = TyFunX () (fmap argTy args) (typeOf returnType)
         in Just (name, funTy)
    argTy :: ArgRn -> TypeTc
    argTy (ArgX (_,mut) _ ty) = case mut of
        Mutable -> Mut $ typeOf ty
        Immutable -> typeOf ty

tcProg :: ProgramRn -> TcM ProgramTc
tcProg (ProgramX () defs) = ProgramX () <$> mapM tcDefs defs

-- TODO: Add the function to callstack for more precise error reporting
tcDefs :: DefRn -> TcM (DefX Tc)
tcDefs (Fn info name args rt block) = do
    let arguments = fmap (\(ArgX (_, mut) name ty) -> (name, (typeOf ty, mut))) args
    args <- mapM tcArg args
    modifying variables (\vars -> foldr (uncurry Map.insert) vars arguments)
    block <- locally returnType (const (typeOf rt)) $ tcBlock block
    case block of
        BlockX _ _ (Just expr) -> unify info (typeOf rt) (typeOf expr)
        BlockX _ _ Nothing -> pure ()
    Fn () name args (typeOf rt) <$> applySt block

-- TODO: Break statments must have the same type as the block in loops
tcBlock :: BlockX Rn -> TcM (BlockX Tc)
tcBlock (BlockX _ statements tailExpression) = do
    stmts <- mapM infStmt statements
    expr <- mapM infExpr tailExpression
    pure $ BlockX (maybe Unit typeOf expr) stmts expr

tcArg :: ArgRn -> TcM ArgTc
tcArg (ArgX _ name ty) = pure $ ArgX () name (typeOf ty)

infStmt :: StmtRn -> TcM StmtTc
infStmt stmt = view returnType >>= flip go stmt
  where
    go returnType = \case
        RetX info mbExpr -> do
            case mbExpr of
                Nothing -> do
                    unless (returnType == Unit) (void $ emptyReturnNonUnit info returnType)
                    pure $ RetX Unit Nothing
                Just expr -> do
                    expr <- tcExpr returnType expr
                    pure $ RetX returnType (Just expr)
        SBlockX _ block -> SBlockX () <$> tcBlock block
        BreakX _ expr -> do
            error "TODO: break"
            expr <- mapM infExpr expr
            pure $ BreakX (maybe Unit typeOf expr) expr
        IfX _ condition true false -> do
            condition <- tcExpr Bool condition
            true <- tcBlock true
            false <- mapM tcBlock false
            pure $ IfX Unit condition true false
        WhileX _ expr stmts -> do
            expr <- tcExpr Bool expr
            block <- tcBlock  stmts
            pure $ WhileX Unit expr block
        SExprX () expr -> do
            SExprX () <$> infExpr expr
        StmtX (LoopX _ block) -> do
            block <- tcBlock block
            pure $ StmtX $ LoopX (typeOf block) block

infExpr :: ExprRn -> TcM ExprTc
infExpr = \case
    LitX _ lit -> pure . uncurry LitX $ infLit lit
    VarX (_, bind) name -> do
        ty <- case bind of
            Free -> lookupVarTy name
            Bound -> lookupVarTy name
            Lambda -> lookupVarTy name
            Toplevel -> lookupFunTy name
        meta <- freshMeta
        putSubst $ singleton meta ty
        pure $ VarX ty name
    BinOpX info l op r -> do
        let types = operatorTypes op
        l <- infExpr l
        let ty = typeOf l
        r <- tcExpr ty r
        ty <- pure $ operatorReturnType ty op
        if typeOf r `elem` types
            then pure $ BinOpX ty l op r
            else do
                ty <- invalidOperatorType info op (typeOf r)
                pure $ BinOpX ty l op r
    AppX info l r -> do
        l <- infExpr l
        -- TODO: Simplify this logic
        -- TODO: Check for mutable argument stuff
        case typeOf l of
            TyFunX () argTys retTy -> do
                let argTysLength = length argTys
                let rLength = length r
                if
                    | argTysLength < rLength -> do
                        retTy <- tooManyArguments info argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX retTy l r
                    | argTysLength > rLength -> do
                        retTy <- partiallyAppliedFunction info argTysLength rLength
                        r <- mapM infExpr r
                        pure $ AppX retTy l r
                    | otherwise -> do
                        r <- zipWithM tcExpr argTys r
                        pure $ AppX retTy l r
            ty -> do
                retTy <- applyNonFunction info ty
                r <- mapM infExpr r
                pure $ AppX retTy l r
    EStmtX info stmt -> do
        stmt <- infStmt stmt
        case stmt of
            IfX _ cond true false -> do
                ty <- case false of
                    Nothing -> missingElse info
                    Just false -> do
                        unify info (typeOf true) (typeOf false)
                        applySt (typeOf true)
                applySt $ EStmtX () $ IfX ty cond true false
            stmt -> pure $ EStmtX () stmt
    LetX (info, mut, mbty) name expr -> do
        ty <- case mbty of
            Nothing -> TypeX <$> freshMeta
            Just ty -> pure $ typeOf ty
        expr <- tcExpr ty expr
        unify info ty (typeOf expr)
        ty <- applySt ty
        insertVar name ty mut
        applySt $ LetX (Unit, ty) name expr
    AssX (info, bind) name op expr -> do
        (ty, mut) <- case bind of
            Toplevel -> assignNonVariable info name >> pure (Unsolvable, Mutable)
            _ -> lookupVar name
        case mut of
            Immutable -> void $ immutableVariable info name
            Mutable -> pure ()
        expr <- tcExpr ty expr
        unify info ty (typeOf expr)
        ty <- applySt ty
        case op of
            AddAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            SubAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            MulAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            DivAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            ModAssign -> unless (ty `elem` [Int, Double]) (void $ tyExpectedGot info [Int, Double] ty)
            Assign -> pure ()
        pure $ AssX (Unit, ty) name op expr

tcExpr :: TypeTc -> ExprRn -> TcM ExprTc
tcExpr expectedTy = \case
    LitX info lit -> do
        let (ty, lit') = infLit lit
        void $ unify info expectedTy ty
        pure $ LitX ty lit'
    VarX (info, bind) name -> do
        expr <- infExpr (VarX (info, bind) name)
        unify info expectedTy (typeOf expr)
        applySt expr
    BinOpX info l op r -> do
        l <- infExpr l
        r <- tcExpr (typeOf l) r
        let ty = operatorReturnType (typeOf r) op
        unify info expectedTy ty
        applySt $ BinOpX expectedTy l op r
    AppX info fun args -> do
        expr <- infExpr (AppX info fun args)
        unify info expectedTy (typeOf expr)
        applySt expr
    LetX (info, mut, mbty) name expr -> do
        expr <- infExpr (LetX (info, mut, mbty) name expr)
        unify info expectedTy (typeOf expr)
        applySt expr
    AssX (info, bind) name op expr -> do
        expr <- infExpr (AssX (info, bind) name op expr)
        unify info expectedTy (typeOf expr)
        applySt expr
    EStmtX info stmt -> do
        expr <- infExpr (EStmtX info stmt)
        unify info expectedTy (typeOf expr)
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

insertVar :: (MonadState Env m) => Ident -> TypeTc -> Mutability -> m ()
insertVar name ty mut = modifying variables (Map.insert name (ty, mut))

lookupVar :: (MonadState Env m) => Ident -> m (TypeTc, Mutability)
lookupVar name = uses variables (fromJust . Map.lookup name)

lookupVarTy :: (MonadState Env m) => Ident -> m TypeTc
lookupVarTy = fmap fst . lookupVar

lookupVarMut :: (MonadState Env m) => Ident -> m Mutability
lookupVarMut = fmap snd . lookupVar

lookupFunTy :: (MonadReader Ctx m) => Ident -> m TypeTc
lookupFunTy name = views functions (fromJust . Map.lookup name)

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
        RetX ty _ -> ty
        SBlockX _ block -> typeOf block
        BreakX ty _ -> ty
        IfX ty _ _ _ -> ty
        WhileX ty _ _ -> ty
        SExprX _ expr -> typeOf expr
        StmtX (LoopX ty _) -> ty

instance TypeOf ExprTc where
    typeOf = \case
        LitX ty _ -> ty
        VarX ty _ -> ty
        BinOpX ty _ _ _ -> ty
        AppX ty _ _ -> ty
        LetX (ty, _) _ _ -> ty
        AssX (ty,_) _ _ _ -> ty
        EStmtX _ stmt -> typeOf stmt

instance TypeOf BlockTc where
    typeOf (BlockX ty _ _) = ty

instance TypeOf TypeRn where
    typeOf = \case
        TyLitX a b -> TyLitX a b
        TyVarX a b -> TyVarX a b
        TyFunX a b c -> TyFunX a (fmap typeOf b) (typeOf c)

-- | Unify two types. The first argument type *must* the expected one!
unify :: (MonadState Env m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> TypeTc -> m ()
unify info ty1 ty2 = case (ty1, ty2) of
    (Unsolvable, _) -> doneTcError
    (_, Unsolvable) -> doneTcError
    (TypeX meta, ty2) -> putSubst $ singleton meta ty2
    (ty1, TypeX meta) -> putSubst $ singleton meta ty1
    (TyLitX _ lit1, TyLitX _ lit2)
        | lit1 == lit2 -> pure ()
        | otherwise -> void $ tyExpectedGot info [ty1] ty2
    (TyVarX _ name1, TyVarX _ name2)
        | name1 == name2 -> pure ()
        | otherwise -> void $ tyExpectedGot info [ty1] ty2
    (TyFunX _ l1 r1, TyFunX _ l2 r2) -> do
        unless (length l1 == length l2) (void $ tyExpectedGot info [ty1] ty2)
        zipWithM_ (unify info) l1 l2
        r1 <- applySt r1
        r2 <- applySt r2
        unify info r1 r2
    (Mut ty1, ty2) -> unify info ty1 ty2
    (ty1, ty2) -> void $ tyExpectedGot info [ty1] ty2
