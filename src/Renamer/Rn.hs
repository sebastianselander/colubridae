{-# LANGUAGE LambdaCase #-}

module Renamer.Rn where

import Control.Monad (foldM)
import Control.Monad.Except
import Data.Set qualified as Set
import Equality (eqAlphaArg)
import Parser.Types
import Relude
import Renamer.Error (RnError (..), onErr)
import Renamer.Monad
import Renamer.Types
import Types
import Utils (listify')

rename :: ProgramPar -> Either Text ProgramRn
rename program@(ProgramX () defs) =
  let toplevels = Set.fromList (snd <$> getDefinitions program)
   in onErr
        $ runGen emptyEnv emptyCtx {definitions = toplevels}
        $ ProgramX ()
        <$> mapM rnDef defs

rnDef :: DefPar -> Gen DefRn
rnDef (Fn pos name arguments returnType statements) = do
  arguments <- uniqueArgs arguments
  returnType <- rnType returnType
  statements <- mapM rnStatement statements
  return $ Fn pos name arguments returnType statements

-- TODO: Implement
rnStatement :: StmtX Par -> Gen (StmtX Rn)
rnStatement = \case
  RetX a b -> do
    b' <- mapM rnExpr b
    pure $ RetX a b'
  BlockX a stmts -> BlockX a <$> mapM rnStatement stmts
  BreakX a expr -> do
    b' <- mapM rnExpr expr
    pure $ BreakX a b'
  IfX a b true false -> do
    b <- rnExpr b
    true <- mapM rnStatement true
    false <- mapM (mapM rnStatement) false
    pure $ IfX a b true false
  WhileX a b stmts -> do
      b <- rnExpr b
      stmts <- mapM rnStatement stmts
      pure $ WhileX a b stmts
  LetX a name expr -> do
      name <- rnName name
      expr <- rnExpr expr
      pure $ LetX a name expr
  SExprX a b -> do
      b <- rnExpr b
      pure $ SExprX a b
  StmtX a -> pure $ StmtX a

rnName :: Ident -> Gen Ident
rnName = pure

rnExpr :: ExprX Par -> Gen (ExprX Rn)
rnExpr = undefined

getDefinitions :: ProgramPar -> [(SourceInfo, Ident)]
getDefinitions = listify' fnName
 where
  fnName :: DefPar -> Maybe (SourceInfo, Ident)
  fnName (Fn info name _ _ _) = Just (info, name)

uniqueArgs :: (MonadError RnError m) => [ArgPar] -> m [ArgRn]
uniqueArgs = foldM f mempty
 where
  f :: (MonadError RnError m) => [ArgRn] -> ArgPar -> m [ArgRn]
  f seen arg@(ArgX info name ty) = do
    when (any (eqAlphaArg arg) seen) (conflictingDefinitionArgument info name)
    ty <- rnType ty
    pure (ArgX info name ty : seen)

rnType :: (Monad m) => TypePar -> m TypeRn
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
