{-# LANGUAGE LambdaCase #-}

module Frontend.Renamer.Rn (rename) where

import Control.Arrow (left)
import Control.Monad (foldM)
import Control.Monad.Except
import Data.Set qualified as Set
import Equality (eqAlphaArg)
import Frontend.Parser.Types
import Relude
import Frontend.Error (Report (..), RnError (..))
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Types
import Utils (impossible, listify')

rename :: ProgramPar -> Either Text ProgramRn
rename = left report . runGen emptyEnv emptyCtx . rnProgram

rnProgram :: ProgramPar -> Gen ProgramRn
rnProgram program@(ProgramX a defs) = do
  let toplevels = getDefinitions program
  uniqueDefs toplevels
  ProgramX a <$> mapM rnDef defs

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
        =<< bound variable
    expr <- rnExpr expr
    pure $ AssX bind name expr
  SExprX a b -> do
    b <- rnExpr b
    pure $ SExprX a b
  StmtX a -> pure $ StmtX a

rnExpr :: ExprPar -> Gen ExprRn
rnExpr = \case
  LitX info lit -> LitX info <$> rnLit lit
  VarX info variable -> do
    (bind, name) <-
      maybe (unboundVariable info variable) pure
        =<< bound variable
    pure $ case bind of
      Bound -> BoundVar info name
      Free -> FreeVar info name
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

rnType :: Monad m => TypePar -> m TypeRn
rnType = \case
  UnitX a -> return $ UnitX a
  IntX a -> pure $ IntX a
  StringX a -> pure $ StringX a
  DoubleX a -> pure $ DoubleX a
  CharX a -> pure $ CharX a
  BoolX a -> pure $ BoolX a
  TyVarX a name -> pure $ TyVarX a name
  TyFunX a b c -> do
    b <- rnType b
    c <- rnType c
    pure $ TyFunX a b c
