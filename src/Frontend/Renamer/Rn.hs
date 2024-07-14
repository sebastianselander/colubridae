{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Renamer.Rn (rename) where

import Control.Lens (locally)
import Control.Monad (foldM)
import Control.Monad.Validate (MonadValidate)
import Data.Set qualified as Set
import Frontend.Builtin (builtInNames)
import Frontend.Error
import Frontend.Parser.Types
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Frontend.Types
import Names (Ident (..), Names, mkNames)
import Relude
import Utils (listify')

rename :: ProgramPar -> Either [RnError] (ProgramRn, Names)
rename = runGen emptyEnv emptyCtx . rnProgram

rnProgram :: ProgramPar -> Gen (ProgramRn, Names)
rnProgram program@(ProgramX a defs) = do
    let toplevels = getDefinitions program
    uniqueDefs toplevels
    let toplevelSet = Set.fromList $ fmap snd toplevels
    defs <- locally definitions (Set.union toplevelSet) (mapM rnDef defs)
    names <- names
    pure (ProgramX a defs, mkNames names)

uniqueDefs :: (MonadValidate [RnError] m) => [(SourceInfo, Ident)] -> m ()
uniqueDefs = go builtInNames
  where
    go :: (MonadValidate [RnError] m) => Set Ident -> [(SourceInfo, Ident)] -> m ()
    go _ [] = pure ()
    go seen ((info, name) : xs) =
        if Set.member name seen
            then duplicateToplevels info name
            else go (Set.insert name seen) xs

rnDef :: DefPar -> Gen DefRn
rnDef (Fn pos name arguments returnType block) = do
    arguments <- rnArgs arguments
    returnType <- rnType returnType
    statements <- rnBlock block
    return $ Fn pos name arguments returnType statements

rnBlock :: BlockPar -> Gen BlockRn
rnBlock (BlockX a stmts expr) =
    uncurry (BlockX a) <$> newContext ((,) <$> mapM rnStatement stmts <*> mapM rnExpr expr)

rnStatement :: StmtPar -> Gen StmtRn
rnStatement = \case
    SExprX a b -> do
        b <- rnExpr b
        pure $ SExprX a b

rnExpr :: ExprPar -> Gen ExprRn
rnExpr = \case
    LitX info lit -> LitX info <$> rnLit lit
    VarX info variable -> do
        (bind, name) <-
            maybe ((Free, Ident "unbound") <$ unboundVariable info variable) pure
                =<< maybe (fmap (Toplevel,) <$> boundFun variable) (pure . Just)
                =<< maybe (fmap (Free,) <$> boundArg variable) (pure . Just)
                =<< boundVar variable
        pure $ VarX (info, bind) name
    PrefixX info op expr -> PrefixX info op <$> rnExpr expr
    BinOpX info l op r -> do
        l <- rnExpr l
        r <- rnExpr r
        pure $ BinOpX info l op r
    AppX info l args -> do
        l <- rnExpr l
        args <- mapM rnExpr args
        pure $ AppX info l args
    LetX (info, mut, ty) name expr -> do
        expr <- rnExpr expr
        name' <- insertVar name
        ty <- mapM rnType ty
        pure $ LetX (info, mut, ty) name' expr
    AssX info variable op expr -> do
        (bind, name) <-
            maybe ((Free, Ident "unbound") <$ unboundVariable info variable) pure
                =<< boundVar variable
        expr <- rnExpr expr
        pure (AssX (info, bind) name op expr)
    RetX a b -> do
        b' <- mapM rnExpr b
        pure $ RetX a b'
    EBlockX info block -> EBlockX info <$> rnBlock block
    BreakX a expr -> do
        b' <- mapM rnExpr expr
        pure $ BreakX a b'
    IfX a b true false -> do
        b <- rnExpr b
        true <- newContext $ rnBlock true
        false <- newContext $ mapM rnBlock false
        pure $ IfX a b true false
    WhileX a b block -> do
        b <- rnExpr b
        stmts <- newContext $ rnBlock block
        pure $ WhileX a b stmts
    LoopX info block -> LoopX info <$> rnBlock block
    LamX info args body -> do
        args <- reverse <$> rnLamArgs args
        body <- newContext $ rnExpr body
        pure $ LamX info args body

rnLamArgs :: (MonadState Env m, MonadValidate [RnError] m) => [LamArgPar] -> m [LamArgRn]
rnLamArgs = foldM f mempty
  where
    f :: (MonadState Env m, MonadValidate [RnError] m) => [LamArgRn] -> LamArgPar -> m [LamArgRn]
    f seen arg@(LamArgX (info, mut, ty) name) = do
        when (any (eqAlphaLamArg arg) seen) (conflictingDefinitionArgument info name)
        name <- insertArg name
        ty <- mapM rnType ty
        pure (LamArgX (info, mut, ty) name : seen)


rnLit :: LitPar -> Gen LitRn
rnLit = \case
    IntLitX info lit -> pure $ IntLitX info lit
    DoubleLitX info lit -> pure $ DoubleLitX info lit
    StringLitX info lit -> pure $ StringLitX info lit
    CharLitX info lit -> pure $ CharLitX info lit
    BoolLitX info lit -> pure $ BoolLitX info lit
    UnitLitX info -> pure $ UnitLitX info

getDefinitions :: ProgramPar -> [(SourceInfo, Ident)]
getDefinitions = listify' fnName
  where
    fnName :: DefPar -> Maybe (SourceInfo, Ident)
    fnName (Fn info name _ _ _) = Just (info, name)

rnArgs :: (MonadState Env m, MonadValidate [RnError] m) => [ArgPar] -> m [ArgRn]
rnArgs = fmap reverse . foldlM f mempty
  where
    f :: (MonadState Env m, MonadValidate [RnError] m) => [ArgRn] -> ArgPar -> m [ArgRn]
    f seen arg@(ArgX (info, mut) name ty) = do
        when (any (eqAlphaArg arg) seen) (conflictingDefinitionArgument info name)
        name <- insertArg name
        ty <- rnType ty
        pure (ArgX (info, mut) name ty : seen)

eqAlphaArg :: forall a b. ArgX a -> ArgX b -> Bool
eqAlphaArg (ArgX _ name1 _) (ArgX _ name2 _) = name1 == name2

eqAlphaLamArg :: forall a b. LamArgX a -> LamArgX b -> Bool
eqAlphaLamArg (LamArgX _ name1) (LamArgX _ name2) = name1 == name2

rnType :: (Monad m) => TypePar -> m TypeRn
rnType = pure . coerceType
