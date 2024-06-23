{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Tc where

import Control.Arrow (left)
import Control.Lens.Getter (use, uses, views)
import Control.Lens.Operators ((+=))
import Control.Lens.Setter (modifying)
import Control.Lens.TH
import Control.Monad.Validate (MonadValidate, Validate, runValidate)
import Data.Map.Strict qualified as Map
import Data.Text (intercalate)
import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Typechecker.Types
import Relude hiding (intercalate)
import Relude.Unsafe (fromJust)
import Types
import Utils (impossible)

data Env = Env {_variables :: Map Ident TypeTc, _freshCounter :: Int}
    deriving (Show)

newtype Ctx = Ctx {_functions :: Map Ident TypeTc}
    deriving (Show)

$(makeLenses ''Env)
$(makeLenses ''Ctx)

newtype TcM a = Tc {runTc :: StateT Env (ReaderT Ctx (Validate [TcError])) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadValidate [TcError], MonadState Env)

tc :: ProgramRn -> Either Text ProgramTc
tc =
    left (intercalate "\n\n" . fmap report)
        . runValidate
        . flip runReaderT initCtx
        . flip evalStateT initEnv
        . runTc
        . tcProg
  where
    initCtx = Ctx mempty
    initEnv = Env mempty 0

tcProg :: ProgramRn -> TcM ProgramTc
tcProg (ProgramX () defs) = ProgramX () <$> mapM tcDefs defs

-- TODO: Add the function to callstack for more precise error reporting
tcDefs :: DefRn -> TcM (DefX Tc)
tcDefs (Fn info name args returnType block) = do
    args <- mapM tcArg args
    let arguments = fmap (\(ArgX _a name ty) -> (name, ty)) args
    modifying variables (\vars -> foldr (uncurry Map.insert) vars arguments)
    stmts <- infBlock (typeOf returnType) block
    pure $ Fn () name args (typeOf returnType) stmts

infBlock :: TypeTc -> BlockX Rn -> TcM (BlockX Tc)
infBlock = undefined

tcArg :: ArgRn -> TcM ArgTc
tcArg (ArgX (_, mut) name ty) = pure $ ArgX mut name (typeOf ty)

infStmt :: TypeTc -> StmtRn -> TcM StmtTc
infStmt returnType = \case
    RetX info mbExpr -> do
        case mbExpr of
            Nothing -> do
                unless (returnType == Unit) (void $ emptyReturnNonUnit info returnType)
                pure $ RetX Unit Nothing
            Just expr -> do
                expr <- tcExpr [returnType] expr
                pure $ RetX returnType (Just expr)
    SBlockX info block -> undefined
    BreakX info _ -> undefined
    IfX info _ _ _ -> undefined
    WhileX info expr stmts -> do
        expr <- tcExpr [Bool] expr
        stmts <- infBlock returnType stmts
        pure $ WhileX Unit expr stmts
    LetX (info, mut, mbty) _ _ -> undefined
    AssX info _ _ -> undefined
    SExprX _ expr -> undefined
    StmtX absurd -> undefined

infExpr :: ExprRn -> TcM ExprTc
infExpr = \case
    LitX _ lit -> pure . uncurry LitX $ infLit lit
    VarX info name -> undefined
    BinOpX info l Add r -> do
        l <- tcExpr [Int, Double] l
        let ty = typeOf l
        r <- tcExpr [ty] r
        pure $ BinOpX ty l Add r
    BinOpX info l op r -> undefined
    AppX {} -> undefined
    EStmtX _ _ -> undefined
    ExprX _ -> undefined

tcExpr :: [TypeTc] -> ExprRn -> TcM ExprTc
tcExpr types = \case
    LitX info lit -> do
        let (ty, lit') = infLit lit
        unless (ty `elem` types) (void $ tyExpectedGot info types ty)
        pure $ LitX ty lit'
    VarX info name -> undefined
    BinOpX info l Add r -> do
        l <- tcExpr [Int, Double] l
        let ty = typeOf l
        r <- tcExpr [ty] r
        pure $ BinOpX ty l Add r
    BinOpX info l op r -> undefined
    AppX {} -> undefined
    EStmtX _ _ -> undefined
    ExprX _ -> undefined

infLit :: LitRn -> (TypeTc, LitTc)
infLit = \case
    IntLitX info n -> (Int, IntLitX info n)
    DoubleLitX info n -> (Double, DoubleLitX info n)
    StringLitX info s -> (String, StringLitX info s)
    CharLitX info c -> (Char, CharLitX info c)
    BoolLitX info b -> (Bool, BoolLitX info b)
    UnitLitX info -> (Unit, UnitLitX info)

lookupVar :: (MonadState Env m) => Ident -> m (Maybe TypeTc)
lookupVar name = uses variables (Map.lookup name)

lookupFun :: (MonadReader Ctx m) => Ident -> m TypeTc
lookupFun name = views functions (fromJust . Map.lookup name)

freshMeta :: (MonadState Env m) => m TypeTc
freshMeta = do
    n <- use freshCounter
    freshCounter += 1
    pure $ TypeX $ Meta n

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
        LetX (_, ty) _ _ -> ty
        AssX ty _ _ -> ty
        SExprX _ expr -> typeOf expr
        StmtX absurd -> impossible absurd

instance TypeOf ExprTc where
    typeOf = \case
        LitX ty _ -> ty
        VarX ty _ -> ty
        BinOpX ty _ _ _ -> ty
        AppX ty _ _ -> ty
        EStmtX _ stmt -> typeOf stmt
        ExprX absurd -> impossible absurd

instance TypeOf BlockTc where
    typeOf (BlockX ty _ _) = ty

instance TypeOf TypeRn where
    typeOf = \case
        TyLitX a b -> TyLitX a b
        TyVarX a b -> TyVarX a b
        TyFunX a b c -> TyFunX a (typeOf b) (typeOf c)
        TypeX absurd -> impossible absurd
