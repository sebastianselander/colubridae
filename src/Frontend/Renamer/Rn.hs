{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Frontend.Renamer.Rn (rename) where

import Control.Lens (locally, modifying, view)
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign)
import Control.Monad.Validate (MonadValidate (dispute, refute))
import Data.List.NonEmpty ((<|))
import Relude hiding (intercalate)

import Data.Map qualified as Map
import Data.Set qualified as Set

import Frontend.Builtin (builtins)
import Frontend.Error
import Frontend.Parser.Types
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Frontend.Types
import Frontend.Utils (isUnique)
import Names (ExplicitName (EName), Ident (..), Namespace)
import Utils (listify')

import Frontend.Builtin qualified as Builtins
import Names qualified


{-

Rewrite:
\* Program should be [Module] instead of [Def]
\* Have the renamer make several passes of the ast
    - Gather all names
    - Check no duplicates
    - Apply renaming
        - store the old name in the node
        - make namespaces explicit on all names
\* We probably need to parameterize the ast type with name like this:
    - Program Ident Par
    - Program (Namespace, Ident) Rn
-}

rename :: Set Namespace -> Set ExplicitName -> ProgramPar -> Either [RnError] ProgramRn
rename namespaces symbols prg@(Program namespace defs) =
    runGen
        (emptyEnv symbols)
        (createCtx namespace builtins namespaces types)
        $ rnProgram prg
  where
    types =
        Set.fromList
            (mapMaybe (\case DefAdt (Adt _loc name _cons) -> Just (EName namespace name name); _ -> Nothing) defs)


rnProgram :: ProgramPar -> Gen ProgramRn
rnProgram program@(Program namespace defs) = do
    let functions = getFunctionNames program
    let adts = getAdtNames program
    let imported = getImportedNames program
    uniqueDefs namespace adts
    uniqueDefs namespace (functions <> imported)
    let toplevelSet = Map.fromList $ fmap (\(_, name) -> (name, EName namespace name name)) functions
    defs <- locally localDefinitions (Map.union toplevelSet) (mapM rnDef (sortDefs defs))
    pure (Program namespace defs)


-- Make sure imports are placed first.
sortDefs :: [Def name a] -> [Def name a]
sortDefs = sortBy f
  where
    f :: Def name a -> Def name a -> Ordering
    f (DefImport _) (DefImport _) = EQ
    f (DefImport _) _ = LT
    f (DefAdt _) (DefImport _) = GT
    f (DefAdt _) (DefAdt _) = EQ
    f (DefAdt _) _ = EQ
    f (DefX _) (DefFn _) = LT
    f (DefX _) (DefX _) = EQ
    f (DefX _) _ = GT
    f (DefFn _) (DefFn _) = EQ
    f (DefFn _) _ = GT


uniqueDefs :: (MonadValidate [RnError] m) => Namespace -> [(SourceInfo, Ident)] -> m ()
uniqueDefs namespace = go (Set.map (.name) $ Builtins.names builtins)
  where
    go :: (MonadValidate [RnError] m) => Set Ident -> [(SourceInfo, Ident)] -> m ()
    go _ [] = pure ()
    go seen ((info, name) : xs) =
        if Set.member name seen
            then duplicateToplevels info (EName namespace name name)
            else go (Set.insert name seen) xs


rnFunction :: FnPar -> Gen FnRn
rnFunction (Fn pos name tyParams arguments returnType block) = do
    resetArgs
    case isUnique tyParams of
        Nothing -> pure ()
        Just (loc, duped) -> dispute [ConflictingTypeParameter loc duped]
    arguments <- rnArgs tyParams arguments
    returnType <- renameType tyParams returnType
    statements <- rnBlock tyParams block
    namespace <- view namespace
    return $ Fn pos (EName namespace name name) tyParams arguments returnType statements


rnDef :: DefPar -> Gen DefRn
rnDef (DefFn fn) = DefFn <$> rnFunction fn
rnDef (DefAdt adt) = DefAdt <$> rnAdt adt
rnDef (DefImport imp) = DefImport <$> rnImport imp


{-|
Transforms all non-explicit imports to explicit imports.

`import foo.bar (baz)` is transformed to `import foo.bar (baz)` and `baz`
    is transformed to `foo.bar.baz` at the usage sites.

Transforms `import foo.bar as baz` to `import foo.bar (f)`
    if `baz.f` is used somewhere in the program,
    `baz.f` is in turn transformed to `foo.bar.f` the usage sites
Transforms `import foo.bar` to `import foo.bar (f)`
    if `foo.bar.f` is used somewhere in the program.
-}
rnImport :: ImportPar -> Gen ImportRn
rnImport (ImportExplicit loc namespace symbols) = do
    exist <- doesNamespaceExist namespace
    unless exist $ unboundImport loc namespace
    modifying
        importedDefinitions
        (\acc -> foldr (\name -> Set.insert (EName namespace name name)) acc symbols)
    pure (ImportExplicit loc namespace (fmap (\name -> EName namespace name name) symbols))
rnImport (XImport extraimport) = rnExtraImport extraimport
  where
    rnExtraImport :: ExtraImports SourceInfo -> Gen ImportRn
    rnExtraImport (ImportAs namespace name loc) = do
        exist <- doesNamespaceExist namespace
        unless exist $ unboundImport loc namespace
        insertImportName name namespace
        defs <- use importedDefinitions
        let symbols = sort $ foldr (\name acc -> if namespace == name.namespace then name : acc else acc) [] defs
        modifying
            importedDefinitions
            (\acc -> foldr Set.insert acc symbols)
        pure (ImportExplicit loc namespace symbols)
    rnExtraImport (ImportQualified namespace loc) = do
        exist <- doesNamespaceExist namespace
        unless exist $ unboundImport loc namespace
        defs <- use importedDefinitions
        let symbols = sort $ foldr (\name acc -> if namespace == name.namespace then name : acc else acc) [] defs
        modifying
            importedDefinitions
            (\acc -> foldr Set.insert acc symbols)
        pure (ImportExplicit loc namespace symbols)


rnAdt :: AdtPar -> Gen AdtRn
rnAdt (Adt loc name constructors) = do
    namespace <- view namespace
    Adt loc (EName namespace name name) <$> mapM rnConstructor constructors


rnConstructor :: ConstructorPar -> Gen ConstructorRn
rnConstructor con = do
    namespace <- view namespace
    case con of
        EnumCons loc name -> checkAndinsertConstrutor loc name >> pure (EnumCons loc (EName namespace name name))
        FunCons loc name types -> do
            checkAndinsertConstrutor loc name
                >> FunCons loc (EName namespace name name)
                <$> mapM (renameType emptyTyParamList) types


rnBlock :: TyParamList -> BlockPar -> Gen BlockRn
rnBlock tyParams (Block a stmts expr) =
    uncurry (Block a)
        <$> newContext ((,) <$> mapM (rnStatement tyParams) stmts <*> mapM (rnExpr tyParams) expr)


rnStatement :: TyParamList -> StmtPar -> Gen StmtRn
rnStatement tyParams = \case
    SExpr a b -> do
        b <- rnExpr tyParams b
        pure $ SExpr a b


rnExpr :: TyParamList -> ExprPar -> Gen ExprRn
rnExpr tyParams = goRnExpr
  where
    goRnExpr :: ExprPar -> Gen ExprRn
    goRnExpr = \case
        Lit info lit -> Lit info <$> rnLit lit
        Var (info, ns) name -> do
            freeVarsAllowed <- view freeVarsAllowed
            namespace <- view namespace
            mby <- case ns of
                Just namespace -> do
                    imported <- boundImported namespace name
                    builtin <- lookupBuiltin namespace name
                    pure (imported <|> builtin)
                Nothing -> do
                    var <- boundVar name
                    arg <- boundArg name
                    fun <- boundFun name
                    con <- boundCons name
                    pure $ var <|> arg <|> fun <|> con
            (bind, name) <- maybe (unboundVariable' info (EName namespace name name)) pure mby
            case bind of
                Free | not freeVarsAllowed -> freeVarsNotAllowed info name
                _ -> pure ()
            pure $ Var (info, bind) name
        Prefix info op expr -> Prefix info op <$> goRnExpr expr
        BinOp info l op r -> do
            l <- goRnExpr l
            r <- goRnExpr r
            pure $ BinOp info l op r
        App info l args -> do
            l <- goRnExpr l
            args <- mapM goRnExpr args
            pure $ App info l args
        Let (info, ty) name expr -> do
            expr <- goRnExpr expr
            name' <- insertVar name
            ty <- mapM (renameType tyParams) ty
            pure $ Let (info, ty) name' expr
        Ass info name op expr -> do
            namespace <- view namespace
            mby_var <- boundVar name
            mby_arg <- boundArg name
            (boundedness, name) <-
                fromMaybe
                    ( (Free, EName namespace (Ident "unbound") (Ident "unbound"))
                        <$ unboundVariable info (EName namespace name name)
                    )
                    $ fmap pure mby_var
                    <|> fmap pure mby_arg

            expr <- goRnExpr expr
            pure (Ass (info, boundedness) name op expr)
        Ret a b -> do
            b' <- mapM goRnExpr b
            pure $ Ret a b'
        EBlock info block -> EBlock info <$> rnBlock tyParams block
        Break a expr -> do
            b' <- mapM goRnExpr expr
            pure $ Break a b'
        If a b true false -> do
            b <- goRnExpr b
            true <- newContext $ rnBlock tyParams true
            false <- newContext $ mapM (rnBlock tyParams) false
            pure $ If a b true false
        While a b block -> do
            b <- goRnExpr b
            stmts <- newContext $ rnBlock tyParams block
            pure $ While a b stmts
        Loop info block -> Loop info <$> rnBlock tyParams block
        Lam info args body -> do
            oldArgs <- use arguments
            modifying arguments (mempty <|)
            args <- rnLamArgs tyParams args
            body <- locally freeVarsAllowed (const False) $ newContext $ goRnExpr body
            assign arguments oldArgs
            pure $ Lam info args body
        Match info scrutinee arms -> do
            scrutinee <- goRnExpr scrutinee
            arms <- mapM (rnMatchArm tyParams) arms
            pure $ Match info scrutinee arms


rnMatchArm :: TyParamList -> MatchArmPar -> Gen MatchArmRn
rnMatchArm tyParams (MatchArm loc pat body) = newContext $ do
    pat <- rnPattern pat
    body <- rnExpr tyParams body
    pure $ MatchArm loc pat body


rnPattern :: PatternPar -> Gen PatternRn
rnPattern = fmap snd . go mempty
  where
    go :: [Ident] -> PatternPar -> Gen ([Ident], PatternRn)
    go seen pat = do
        namespace <- view namespace
        case pat of
            PVar loc varName -> do
                when
                    (varName `elem` seen)
                    (conflictingDefinitionArgument loc (EName namespace varName varName))
                name <- insertVar varName
                pure (varName : seen, PVar loc name)
            PEnumCon loc conName -> do
                pure ([], PEnumCon loc (EName namespace conName conName))
            PFunCon loc conName pats -> do
                (seen, pats) <- go' seen pats
                pure (seen, PFunCon loc (EName namespace conName conName) pats)
              where
                go' :: [Ident] -> [PatternPar] -> Gen ([Ident], [PatternRn])
                go' seen [] = pure (seen, [])
                go' seen (x : xs) = do
                    (seen', pat) <- go seen x
                    (seen'', pats) <- go' (seen <> seen') xs
                    pure (seen <> seen' <> seen'', pat : pats)


rnLamArgs ::
    (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
    TyParamList -> [LamArgPar] -> m [LamArgRn]
rnLamArgs tyParams = fmap (reverse . snd) . foldlM f mempty
  where
    f ::
        (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
        ([Ident], [LamArgRn]) ->
        LamArgPar ->
        m ([Ident], [LamArgRn])
    f (seen, acc) (LamArg (info, ty) name) = do
        namespace <- view namespace
        let seen' = name : seen
        when (name `elem` seen) (conflictingDefinitionArgument info (EName namespace name name))
        name <- insertArg namespace name
        ty <- mapM (renameType tyParams) ty
        pure (seen', LamArg (info, ty) name : acc)


rnLit :: LitPar -> Gen LitRn
rnLit = \case
    IntLit info lit -> pure $ IntLit info lit
    DoubleLit info lit -> pure $ DoubleLit info lit
    StringLit info lit -> pure $ StringLit info lit
    CharLit info lit -> pure $ CharLit info lit
    BoolLit info lit -> pure $ BoolLit info lit
    UnitLit info -> pure $ UnitLit info


getAdtNames :: ProgramPar -> [(SourceInfo, Ident)]
getAdtNames = listify' adtName
  where
    adtName :: AdtPar -> Maybe (SourceInfo, Ident)
    adtName (Adt info name _) = Just (info, name)


getFunctionNames :: ProgramPar -> [(SourceInfo, Ident)]
getFunctionNames = listify' fnName
  where
    fnName :: FnPar -> Maybe (SourceInfo, Ident)
    fnName (Fn info name _ _ _ _) = Just (info, name)


getImportedNames :: ProgramPar -> [(SourceInfo, Ident)]
getImportedNames = concat . listify' importName
  where
    importName :: ImportPar -> Maybe [(SourceInfo, Ident)]
    importName (ImportExplicit loc _namespace names) = Just (fmap (loc,) names)
    importName (XImport ImportAs {}) = Nothing
    importName (XImport (ImportQualified {})) = Nothing


rnArgs ::
    (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
    TyParamList -> [ArgPar] -> m [ArgRn]
rnArgs tyParams = fmap (reverse . snd) . foldlM f mempty
  where
    f ::
        (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
        ([Ident], [ArgRn]) ->
        ArgPar ->
        m ([Ident], [ArgRn])
    f (seen, acc) (Arg info name ty) = do
        namespace <- view namespace
        let seen' = name : seen
        when (name `elem` seen) (conflictingDefinitionArgument info (EName namespace name name))
        name <- insertArg namespace name
        ty <- renameType tyParams ty
        pure (seen', Arg info name ty : acc)


renameType :: (MonadReader Ctx m, MonadValidate [RnError] m) => TyParamList -> TypePar -> m TypeRn
renameType typeParams ty = do
    userDefinedTypes <- view userDefinedTypes
    namespace <- view namespace
    case ty of
        TyCon _ name -> pure (TyCon NoExtField (EName namespace name name))
        TypeVar _ tyvar -> pure (TypeVar NoExtField tyvar)
        TyLit _ lit -> pure (TyLit NoExtField lit)
        TyFun _ args ret -> TyFun NoExtField <$> mapM (renameType typeParams) args <*> renameType typeParams ret
        Type unresolved@(UnresolvedType loc _)
            | isTypeVar unresolved typeParams -> pure (TypeVar NoExtField (tyVarOf unresolved))
            | let name = EName namespace (nameOf unresolved) (nameOf unresolved)
            , Set.member name userDefinedTypes ->
                pure (TyCon NoExtField name)
            | let name = EName namespace (nameOf unresolved) (nameOf unresolved)
            , otherwise ->
                refute [UnboundType loc name]
