{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Renamer.Rn (rename) where

import Control.Lens (locally)
import Control.Monad.Validate (MonadValidate, Validate)
import Data.Set qualified as Set
import Error.Diagnose qualified as Diagnose
import Frontend.Builtin (builtInNames)
import Frontend.Error
import Frontend.Parser.Types
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Frontend.Types
import Names (Ident (..), Names, mkNames)
import Relude
import Utils (listify')

rename :: ProgramPar -> Validate [Diagnose.Report Text] (ProgramRn, Names)
rename program = runGen emptyEnv emptyCtx $ rnProgram program

rnProgram :: ProgramPar -> Gen (ProgramRn, Names)
rnProgram program@(ProgramX a defs) = do
    let functions = getFunctionNames program
    let adts = getAdtNames program
    uniqueDefs adts
    uniqueDefs functions
    let toplevelSet = Set.fromList $ fmap snd functions
    defs <- locally definitions (Set.union toplevelSet) (mapM rnDef defs)
    names <- names
    pure (ProgramX a defs, mkNames names)

uniqueDefs :: (MonadValidate [Diagnose.Report Error] m) => [(Diagnose.Position, Ident)] -> m ()
uniqueDefs = go builtInNames
  where
    go :: (MonadValidate [Diagnose.Report Error] m) => Set Ident -> [(Diagnose.Position, Ident)] -> m ()
    go _ [] = pure ()
    go seen ((info, name) : xs) =
        if Set.member name seen
            then undefined
            else go (Set.insert name seen) xs

rnFunction :: FnPar -> Gen FnRn
rnFunction (Fn pos name arguments returnType block) = do
    resetArgs
    arguments <- rnArgs arguments
    returnType <- rnType returnType
    statements <- rnBlock block
    return $ Fn pos name arguments returnType statements

rnDef :: DefPar -> Gen DefRn
rnDef (DefFn fn) = DefFn <$> rnFunction fn
rnDef (DefAdt adt) = DefAdt <$> rnAdt adt

rnAdt :: AdtPar -> Gen AdtRn
rnAdt (AdtX loc name constructors) = AdtX loc name <$> mapM rnConstructor constructors

rnConstructor :: ConstructorPar -> Gen ConstructorRn
rnConstructor = \case
    EnumCons loc name -> checkAndinsertConstrutor loc name >> pure (EnumCons loc name)
    FunCons loc name types ->
        checkAndinsertConstrutor loc name
            >> FunCons loc name
            <$> mapM rnType types

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
            maybe
                ( (Free, Ident "unbound")
                    <$ reportSimple
                        (UnboundVariable variable)
                        [
                            ( info
                            , Diagnose.This (MoreInfo "not found in this scope")
                            )
                        ]
                )
                pure
                =<< maybe (fmap (Constructor,) <$> boundCons variable) (pure . Just)
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
    LetX (info, ty) name expr -> do
        expr <- rnExpr expr
        name' <- insertVar name
        ty <- mapM rnType ty
        pure $ LetX (info, ty) name' expr
    AssX info variable op expr -> do
        (bind, name) <-
            maybe
                ( (Free, Ident "unbound")
                    <$ reportSimple
                        (UnboundVariable variable)
                        [
                            ( info
                            , Diagnose.This
                                ( MoreInfo "not found in this scope"
                                )
                            )
                        ]
                )
                pure
                =<< maybe (fmap (Free,) <$> boundArg variable) (pure . Just)
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
        args <- rnLamArgs args
        body <- newContext $ rnExpr body
        pure $ LamX info args body
    MatchX info scrutinee arms -> do
        scrutinee <- rnExpr scrutinee
        arms <- mapM rnMatchArm arms
        pure $ MatchX info scrutinee arms

rnMatchArm :: MatchArmPar -> Gen MatchArmRn
rnMatchArm (MatchArmX loc pat body) = newContext $ do
    pat <- rnPattern pat
    body <- rnExpr body
    pure $ MatchArmX loc pat body

rnPattern :: PatternPar -> Gen PatternRn
rnPattern = fmap snd . go mempty
  where
    go :: [Ident] -> PatternPar -> Gen ([Ident], PatternRn)
    go seen = \case
        PVarX loc varName -> do
            when
                (varName `elem` seen)
                ( reportSimple
                    (ConflictingDefinitionArgument varName)
                    [ (loc, Diagnose.This (MoreInfo "used in a pattern more than once here"))
                    ]
                )
            name <- insertVar varName
            pure (varName : seen, PVarX loc name)
        PEnumConX loc conName -> pure ([], PEnumConX loc conName)
        PFunConX loc conName pats -> do
            (seen, pats) <- go' seen pats
            pure (seen, PFunConX loc conName pats)
          where
            go' :: [Ident] -> [PatternPar] -> Gen ([Ident], [PatternRn])
            go' seen [] = pure (seen, [])
            go' seen (x : xs) = do
                (seen', pat) <- go seen x
                (seen'', pats) <- go' (seen <> seen') xs
                pure (seen <> seen' <> seen'', pat : pats)

rnLamArgs ::
    (MonadState Env m, MonadValidate [Diagnose.Report Error] m) => [LamArgPar] -> m [LamArgRn]
rnLamArgs = fmap (reverse . snd) . foldlM f mempty
  where
    f ::
        (MonadState Env m, MonadValidate [Diagnose.Report Error] m) =>
        ([Ident], [LamArgRn]) ->
        LamArgPar ->
        m ([Ident], [LamArgRn])
    f (seen, acc) (LamArgX (info, ty) name) = do
        let seen' = name : seen
        when
            (name `elem` seen)
            ( reportSimple
                (ConflictingDefinitionArgument name)
                [(info, Diagnose.This (MoreInfo "used in a pattern more than once here"))]
            )
        name <- insertArg name
        ty <- mapM rnType ty
        pure (seen', LamArgX (info, ty) name : acc)

rnLit :: LitPar -> Gen LitRn
rnLit = \case
    IntLitX info lit -> pure $ IntLitX info lit
    DoubleLitX info lit -> pure $ DoubleLitX info lit
    StringLitX info lit -> pure $ StringLitX info lit
    CharLitX info lit -> pure $ CharLitX info lit
    BoolLitX info lit -> pure $ BoolLitX info lit
    UnitLitX info -> pure $ UnitLitX info

getAdtNames :: ProgramPar -> [(Diagnose.Position, Ident)]
getAdtNames = listify' adtName
  where
    adtName :: AdtPar -> Maybe (Diagnose.Position, Ident)
    adtName (AdtX info name _) = Just (info, name)

getFunctionNames :: ProgramPar -> [(Diagnose.Position, Ident)]
getFunctionNames = listify' fnName
  where
    fnName :: FnPar -> Maybe (Diagnose.Position, Ident)
    fnName (Fn info name _ _ _) = Just (info, name)

rnArgs :: (MonadState Env m, MonadValidate [Diagnose.Report Error] m) => [ArgPar] -> m [ArgRn]
rnArgs = fmap (reverse . snd) . foldlM f mempty
  where
    f ::
        (MonadState Env m, MonadValidate [Diagnose.Report Error] m) =>
        ([Ident], [ArgRn]) ->
        ArgPar ->
        m ([Ident], [ArgRn])
    f (seen, acc) (ArgX info name ty) = do
        let seen' = name : seen
        when (name `elem` seen) undefined
        name <- insertArg name
        ty <- rnType ty
        pure (seen', ArgX info name ty : acc)

rnType :: (Monad m) => TypePar -> m TypeRn
rnType = pure . coerceType
