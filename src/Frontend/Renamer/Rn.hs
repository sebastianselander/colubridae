{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend.Renamer.Rn (rename) where

import Control.Lens (locally, modifying, uses, view, views)
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign)
import Control.Monad.Validate (MonadValidate (dispute, refute), runValidate)
import Data.Foldable (foldrM)
import Data.List.NonEmpty ((<|))
import Data.Text.Lazy (unpack)
import Relude hiding (intercalate)
import Text.Pretty.Simple (pShow)

import Data.Map qualified as Map
import Data.Set qualified as Set

import Frontend.Builtin (builtins)
import Frontend.Error
import Frontend.Parser.Types
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Frontend.Types
import Frontend.Utils (isUnique)
import Names (ExplicitName (EName), Ident (..), Namespace (Namespace))
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

-- TODO: Refactor this to be readable...
-- Formatting??
buildSymbolTable :: (MonadValidate [RnError] m) => NonEmpty ModulePar -> m SymbolTable
buildSymbolTable = foldrM fetch mempty
  where
    fetch :: (MonadValidate [RnError] m) => ModulePar -> SymbolTable -> m SymbolTable
    fetch (Module namespace mod) table = do
        let functions = getFunctionNames namespace mod <> getImportedNames mod
            datatypes = getTypeNames namespace mod
            constructors = concat $ getConstructorNames namespace mod
        uniqueNames functions
        uniqueNames constructors
        uniqueNames datatypes
        pure
            $ table
                { symbols =
                    Map.insert
                        namespace
                        ( Symbols
                            { functions = Map.fromList [(func.name, func) | (_, func) <- functions]
                            , datatypes = Map.fromList [(ty.name, ty) | (_, ty) <- datatypes]
                            , constructors = Map.fromList [(con.name, con) | (_, con) <- constructors]
                            }
                        )
                        table.symbols
                }

rename :: ProgramPar -> Either [RnError] ProgramRn
rename (Program NoExtField mods) = do
    tbl <- runValidate $ buildSymbolTable mods
    let results = fmap (rnModule tbl) mods
    case partitionEithers (Relude.toList results) of
        (concat -> [], mod : mods) -> pure (Program NoExtField (mod :| mods))
        (concat -> errors, _) -> Left errors

-- runGen :: Env -> Ctx -> Gen a -> Either [RnError] a
rnModule :: SymbolTable -> ModulePar -> Either [RnError] ModuleRn
rnModule tbl (Module namespace defs) =
    runGen emptyEnv (createCtx tbl namespace builtins) (Module namespace <$> mapM rnDef (sortDefs defs))

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

uniqueNames ::
    (MonadValidate [RnError] m) => [(SourceInfo, ExplicitName)] -> m ()
uniqueNames = go (Set.map (.name) $ Builtins.names builtins)
  where
    go :: (MonadValidate [RnError] m) => Set Ident -> [(SourceInfo, ExplicitName)] -> m ()
    go _ [] = pure ()
    go seen ((info, name) : xs) =
        if Set.member name.name seen
            then duplicateToplevels info name
            else go (Set.insert name.name seen) xs

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
rnImport (ImportExplicit loc namespace imports) = do
    symbols <- views symbolTable (Map.lookup namespace . symbols)
    case symbols of
        Nothing -> do
            unboundImport loc namespace
            pure (ImportExplicit loc namespace [])
        Just symbols -> do
            names <- mapM (findName symbols) imports
            pure (ImportExplicit loc namespace names)
  where
    findName :: (MonadValidate [RnError] m) => Symbols -> Ident -> m ExplicitName
    findName symbols name = do
        let funName = Map.lookup name symbols.functions
        let tyName = Map.lookup name symbols.datatypes
        let conName = Map.lookup name symbols.constructors
        maybe (unboundVariable' loc (EName namespace name name)) pure (funName <|> tyName <|> conName)
rnImport (XImport extraimport) = rnExtraImport extraimport
  where
    rnExtraImport :: ExtraImports SourceInfo -> Gen ImportRn
    rnExtraImport (ImportAs namespace name loc) = do
        symbols <- views symbolTable (Map.lookup namespace . symbols)
        insertImportName name namespace
        case symbols of
            Nothing -> unboundImport' loc namespace
            Just symbols -> do
                let functions = Map.elems symbols.functions
                -- let datatypes = Map.elems symbols.datatypes
                -- let constructors = Map.elems symbols.constructors
                pure (ImportExplicit loc namespace functions)
    rnExtraImport (ImportQualified namespace loc) = do
        symbols <- views symbolTable (Map.lookup namespace . symbols)
        case symbols of
            Nothing -> unboundImport' loc namespace
            Just symbols -> do
                let functions = filter (\name -> name.namespace == namespace) $ Map.elems symbols.functions
                -- let datatypes = Map.elems symbols.datatypes
                -- let constructors = Map.elems symbols.constructors
                pure (ImportExplicit loc namespace functions)

rnAdt :: AdtPar -> Gen AdtRn
rnAdt (Adt loc name@(Ident nm) constructors) = do
    namespace <- view namespace
    let typeNamespace = Namespace (nm :| [])
    Adt loc (EName namespace name name) <$> mapM (rnConstructor typeNamespace) constructors

rnConstructor :: Namespace -> ConstructorPar -> Gen ConstructorRn
rnConstructor typeNamespace con = do
    namespace <- view namespace
    case con of
        EnumCons loc name -> do
            name' <-
                views
                    symbolTable
                    ( Map.lookup name
                        . constructors
                        <=< Map.lookup namespace
                        . symbols
                    )
            EnumCons loc
                <$> maybe
                    ( unboundVariable'
                        loc
                        (EName (namespace <> typeNamespace) name name)
                    )
                    pure
                    name'
        FunCons loc name types -> do
            name' <-
                views
                    symbolTable
                    ( Map.lookup name
                        . constructors
                        <=< Map.lookup namespace
                        . symbols
                    )
            types <- mapM (renameType emptyTyParamList) types
            name <-
                maybe
                    ( unboundVariable'
                        loc
                        (EName (namespace <> typeNamespace) name name)
                    )
                    pure
                    name'
            pure (FunCons loc name types)

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
                    function <- fmap (Function,) <$> lookupFunction namespace name
                    constructor <- fmap (Constructor,) <$> lookupConstructor namespace name
                    builtin <- lookupBuiltin namespace name
                    asNamespace <- uses importName (Map.lookup namespace)
                    ases <- case asNamespace of
                        Just namespace -> do
                            asFunction <- fmap (Function,) <$> lookupFunction namespace name
                            asConstructor <- fmap (Constructor,) <$> lookupConstructor namespace name
                            asBuiltin <- lookupBuiltin namespace name
                            pure [asFunction, asConstructor, asBuiltin]
                        Nothing -> pure []
                    pure (function <|> constructor <|> builtin <|> foldl' (<|>) empty ases)
                Nothing -> do
                    var <- boundVar name
                    arg <- boundArg name
                    fun <- fmap (Function,) <$> lookupFunction namespace name
                    con <- fmap (Constructor,) <$> lookupConstructor namespace name
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

getTypeNames :: Namespace -> [DefPar] -> [(SourceInfo, ExplicitName)]
getTypeNames namespace = listify' adtName
  where
    adtName :: AdtPar -> Maybe (SourceInfo, ExplicitName)
    adtName (Adt info name _) = Just (info, EName namespace name name)

getConstructorNames :: Namespace -> [DefPar] -> [[(SourceInfo, ExplicitName)]]
getConstructorNames namespace = listify' adtName
  where
    adtName :: AdtPar -> Maybe [(SourceInfo, ExplicitName)]
    adtName (Adt _ _ cons) = Just (fmap conName cons)
      where
        conName :: ConstructorPar -> (SourceInfo, ExplicitName)
        conName (EnumCons loc name) = (loc, EName namespace name name)
        conName (FunCons loc name _) = (loc, EName namespace name name)

getFunctionNames :: Namespace -> [DefPar] -> [(SourceInfo, ExplicitName)]
getFunctionNames namespace = listify' fnName
  where
    fnName :: FnPar -> Maybe (SourceInfo, ExplicitName)
    fnName (Fn info name _ _ _ _) = Just (info, EName namespace name name)

getImportedNames :: [DefPar] -> [(SourceInfo, ExplicitName)]
getImportedNames = concat . listify' importName
  where
    importName :: ImportPar -> Maybe [(SourceInfo, ExplicitName)]
    importName (ImportExplicit loc namespace names) = Just [(loc, EName namespace name name) | name <- names]
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
    namespace <- view namespace
    userDefinedTypes <- views symbolTable (fmap datatypes . Map.lookup namespace . symbols)
    case ty of
        TyCon _ name -> pure (TyCon NoExtField (EName namespace name name))
        TypeVar _ tyvar -> pure (TypeVar NoExtField tyvar)
        TyLit _ lit -> pure (TyLit NoExtField lit)
        TyFun _ args ret -> TyFun NoExtField <$> mapM (renameType typeParams) args <*> renameType typeParams ret
        Type unresolved@(UnresolvedType loc _)
            | isTypeVar unresolved typeParams -> pure (TypeVar NoExtField (tyVarOf unresolved))
            | Just types <- userDefinedTypes
            , Just name <- Map.lookup (nameOf unresolved) types ->
                pure (TyCon NoExtField name)
            | let name = EName namespace (nameOf unresolved) (nameOf unresolved)
            , otherwise ->
                refute [UnboundType loc name]
