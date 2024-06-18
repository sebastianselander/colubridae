{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Tc where

import Control.Lens.Getter (use)
import Control.Lens.Operators ((+=))
import Control.Lens.TH
import Control.Monad.Writer (MonadWriter, Writer, runWriter)
import Data.Text (intercalate)
import Frontend.Error (Report (report), TcError)
import Frontend.Renamer.Types
import Frontend.Typechecker.Types
import Relude hiding (intercalate)
import Types

data Env = Env {_variables :: Map Ident TypeTc, _freshCounter :: Int}
  deriving (Show)
$(makeLenses ''Env)

newtype Ctx = Ctx {_functions :: Map Ident TypeTc}
  deriving (Show)

$(makeLenses ''Ctx)

newtype TcM a = Tc {runTc :: StateT Env (ReaderT Ctx (Writer [TcError])) a}
  deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadWriter [TcError], MonadState Env)

tc :: ProgramRn -> Either Text ProgramTc
tc = runReport . runWriter . flip runReaderT initCtx . flip evalStateT initEnv . runTc . tcProg
 where
  runReport :: (ProgramTc, [TcError]) -> Either Text ProgramTc
  runReport (prg, []) = Right prg
  runReport (_, xs) = Left $ intercalate "\n\n" $ fmap report xs
  initCtx = Ctx mempty
  initEnv = Env mempty 0

tcProg :: ProgramRn -> TcM ProgramTc
tcProg (ProgramX () defs) = ProgramX () <$> mapM tcDefs defs

tcDefs :: DefRn -> TcM (DefX Tc)
tcDefs (Fn info name args returnType statements) = undefined

-- TODO: Always assign a meta type everytime we do type inference. Solve the
-- meta variable as soon as we can.
infExpr :: ExprRn -> TcM ExprTc
infExpr = \case
  LitX _ lit -> pure . uncurry LitX $ infLit lit
  VarX _ _ -> undefined
  BinOpX {} -> undefined
  AppX {} -> undefined
  EStmtX _ _ -> undefined
  ExprX _ -> undefined

infLit :: LitRn -> (TypeTc, LitTc)
infLit = \case
  IntLitX info n -> (IntX info, IntLitX () n) 
  DoubleLitX info n -> (DoubleX info, DoubleLitX () n)
  StringLitX info s -> (StringX info, StringLitX () s)
  CharLitX info c -> (CharX info, CharLitX () c)
  BoolLitX info b -> (BoolX info, BoolLitX () b)

freshMeta :: (MonadState Env m) => m TypeTc
freshMeta = do
  n <- use freshCounter
  freshCounter += 1
  pure $ TypeX $ Meta n
