{-# LANGUAGE LambdaCase #-}

module Frontend.Renamer.Rn (rename) where

import Control.Arrow (left)
import Control.Lens (locally)
import Control.Monad (foldM)
import Control.Monad.Except
import Data.Set qualified as Set
import Frontend.Error 
import Frontend.Parser.Types
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Relude
import Types
import Utils (impossible, listify')

rename :: ProgramPar -> Either Text ProgramRn
rename = left report . runGen emptyEnv emptyCtx . rnProgram

rnProgram :: ProgramPar -> Gen ProgramRn
rnProgram program@(ProgramX a defs) = do
  let toplevels = getDefinitions program
  uniqueDefs toplevels
  let toplevelSet = Set.fromList $ fmap snd toplevels
  defs <- locally definitions (const toplevelSet) (mapM rnDef defs)
  pure $ ProgramX a defs

uniqueDefs :: (MonadError RnError m) => [(SourceInfo, Ident)] -> m ()
uniqueDefs = go mempty
 where
  go :: (MonadError RnError m) => Set Ident -> [(SourceInfo, Ident)] -> m ()
  go _ [] = pure ()
  go seen ((info, name) : xs) =
    if Set.member name seen
      then duplicateToplevels info name
      else go (Set.insert name seen) xs

rnDef :: DefPar -> Gen DefRn
rnDef (Fn pos name arguments returnType statements) = do
  arguments <- uniqueArgs arguments
  returnType <- rnType returnType
  statements <- mapM rnStatement statements
  return $ Fn pos name arguments returnType statements

rnStatement :: StmtX Par -> Gen (StmtX Rn)
rnStatement = \case
  RetX a b -> do
    b' <- mapM rnExpr b
    pure $ RetX a b'
  BlockX a stmts -> BlockX a <$> newContext (mapM rnStatement stmts)
  BreakX a expr -> do
    b' <- mapM rnExpr expr
    pure $ BreakX a b'
  IfX a b true false -> do
    b <- rnExpr b
    true <- newContext $ mapM rnStatement true
    false <- newContext $ mapM (mapM rnStatement) false
    pure $ IfX a b true false
  WhileX a b stmts -> do
    b <- rnExpr b
    stmts <- newContext $ mapM rnStatement stmts
    pure $ WhileX a b stmts
  LetX a name expr -> do
    expr <- rnExpr expr
    name' <- insertVar name
    pure $ LetX a name' expr
  AssX info variable expr -> do
    (bind, name) <-
      maybe (unboundVariable info variable) pure
        =<< boundVar variable
    expr <- rnExpr expr
    case bind of
      Bound -> pure $ StmtX (AssRn info (BoundVar name) expr)
      Free -> pure $ StmtX (AssRn info (FreeVar name) expr)
  SExprX a b -> do
    b <- rnExpr b
    pure $ SExprX a b
  StmtX a -> impossible a

rnExpr :: ExprPar -> Gen ExprRn
rnExpr = \case
  LitX info lit -> LitX info <$> rnLit lit
  VarX info variable -> do
    (bind, name) <-
      maybe (unboundVariable info variable) pure
        =<< maybe (fmap (Bound,) <$> boundFun variable) (pure . Just)
        =<< boundVar variable
    pure $ case bind of
      Bound -> ExprX (info, BoundVar name)
      Free -> ExprX (info, FreeVar name)
  BinOpX info l op r -> do
    l <- rnExpr l
    r <- rnExpr r
    pure $ BinOpX info l op r
  AppX info l args -> do
    l <- rnExpr l
    args <- mapM rnExpr args
    pure $ AppX info l args
  EStmtX info stmt -> EStmtX info <$> rnStatement stmt
  ExprX v -> impossible v

rnLit :: LitPar -> Gen LitRn
rnLit = \case
  IntLitX info lit -> pure $ IntLitX info lit
  DoubleLitX info lit -> pure $ DoubleLitX info lit
  StringLitX info lit -> pure $ StringLitX info lit
  CharLitX info lit -> pure $ CharLitX info lit
  BoolLitX info lit -> pure $ BoolLitX info lit

getDefinitions :: ProgramPar -> [(SourceInfo, Ident)]
getDefinitions = listify' fnName
 where
  fnName :: DefPar -> Maybe (SourceInfo, Ident)
  fnName (Fn info name _ _ _) = Just (info, name)

uniqueArgs :: (MonadError RnError m) => [ArgPar] -> m [ArgRn]
uniqueArgs = foldM f mempty
 where
  f :: (MonadError RnError m) => [ArgRn] -> ArgPar -> m [ArgRn]
  f seen arg@(ArgX (info, mut) name ty) = do
    when (any (eqAlphaArg arg) seen) (conflictingDefinitionArgument info name)
    ty <- rnType ty
    pure (ArgX (info, mut) name ty : seen)

eqAlphaArg :: forall a b. ArgX a -> ArgX b -> Bool
eqAlphaArg (ArgX _ name1 _) (ArgX _ name2 _) = name1 == name2

rnType :: (Monad m) => TypePar -> m TypeRn
rnType = \case
  TyLitX a lit -> pure $ TyLitX a lit
  TyVarX a name -> pure $ TyVarX a name
  TyFunX a b c -> do
    b <- rnType b
    c <- rnType c
    pure $ TyFunX a b c
  TypeX a -> pure $ TypeX a
