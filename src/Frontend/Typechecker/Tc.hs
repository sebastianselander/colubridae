{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Tc where

import Control.Lens.Getter (use, uses, views)
import Control.Lens.Operators ((+=))
import Control.Lens.TH
import Control.Monad.Writer (MonadWriter, Writer, runWriter)
import Data.Map.Strict qualified as Map
import Data.Text (intercalate)
import Frontend.Error (Report (report), TcError, tyExpectedGot)
import Frontend.Renamer.Types
import Frontend.Typechecker.Types
import Relude hiding (intercalate)
import Relude.Unsafe (fromJust)
import Types
import Utils (impossible)
import Control.Monad.Validate (Validate, MonadValidate, runValidate)
import Control.Arrow (left)

data Env = Env {_variables :: Map Ident TypeTc, _freshCounter :: Int}
  deriving (Show)

newtype Ctx = Ctx {_functions :: Map Ident TypeTc}
  deriving (Show)

$(makeLenses ''Env)
$(makeLenses ''Ctx)

newtype TcM a = Tc {runTc :: StateT Env (ReaderT Ctx (Validate [TcError])) a}
  deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadValidate [TcError], MonadState Env)

tc :: ProgramRn -> Either Text ProgramTc
tc = left (intercalate "\n\n" . fmap report) . runValidate . flip runReaderT initCtx . flip evalStateT initEnv . runTc . tcProg
 where
  initCtx = Ctx mempty
  initEnv = Env mempty 0

tcProg :: ProgramRn -> TcM ProgramTc
tcProg (ProgramX () defs) = ProgramX () <$> mapM tcDefs defs

tcDefs :: DefRn -> TcM (DefX Tc)
tcDefs (Fn info name args returnType statements) = undefined

infStmt :: StmtRn -> TcM StmtTc
infStmt = undefined

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

instance TypeOf StmtTc where
    typeOf = \case

instance TypeOf ExprTc where
    typeOf = \case
      LitX ty _ -> ty
      VarX ty _ -> ty
      BinOpX ty _ _ _ -> ty
      AppX ty _ _ -> ty
      EStmtX _ stmt -> typeOf stmt
      ExprX absurd -> impossible absurd

