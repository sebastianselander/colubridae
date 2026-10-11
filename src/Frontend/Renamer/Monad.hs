{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Renamer.Monad
    ( Env,
      Ctx,
      Gen,
      newContext,
      boundVar,
      insertVar,
      boundArg,
      boundCons,
      lookupBuiltin,
      createCtx,
      emptyEnv,
      localDefinitions,
      importedDefinitions,
      freeVarsAllowed,
      importName,
      namespace,
      numbering,
      runGen,
      boundFun,
      boundImported,
      insertArg,
      checkAndinsertConstrutor,
      arguments,
      resetArgs,
      insertImportName,
      namespaces,
      doesNamespaceExist,
      userDefinedTypes,
    )
where

import Control.Lens hiding ((<|))
import Control.Monad.Validate (MonadValidate, Validate, runValidate)
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Relude hiding (Map, head)

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Frontend.Builtin (Builtins, isBuiltin)
import Frontend.Error
import Frontend.Typechecker.Types (Tc)
import Frontend.Types (Boundedness (..), SourceInfo)
import Names (ExplicitName (EName), Ident (..), Namespace)


data Env = Env
    { _numbering :: Map Ident Int
    , _scope :: NonEmpty (Map Ident ExplicitName)
    , _arguments :: NonEmpty (Map Ident ExplicitName)
    , _constructors :: Map Ident ExplicitName
    , _importedDefinitions :: Set ExplicitName -- Top level symbols available
    , _importName :: Map Ident Namespace -- as-name to import name (path)
    }
    deriving (Show)


data Ctx = Ctx
    { _freeVarsAllowed :: Bool
    , _localDefinitions :: Map Ident ExplicitName
    , _namespace :: Namespace
    , _builtins :: Builtins Tc
    , _namespaces :: Set Namespace
    , _userDefinedTypes :: Set ExplicitName
    }
    deriving (Show)


$(makeLenses ''Env)
$(makeLenses ''Ctx)


newtype Gen a = Gen {runGen' :: StateT Env (ReaderT Ctx (Validate [RnError])) a}
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadReader Ctx
        , MonadState Env
        , MonadValidate [RnError]
        )


emptyEnv :: Set ExplicitName -> Env
emptyEnv imported =
    Env
        { _numbering = mempty
        , _scope = return mempty
        , _arguments = mempty :| []
        , _constructors = mempty
        , _importedDefinitions = imported
        , _importName = mempty
        }


createCtx ::
    Namespace ->
    Builtins Tc ->
    Set Namespace ->
    Set ExplicitName ->
    Ctx
createCtx = Ctx True mempty


runGen :: Env -> Ctx -> Gen a -> Either [RnError] a
runGen env ctx =
    runValidate
        . flip runReaderT ctx
        . flip evalStateT env
        . runGen'


-- TODO: Does not check for imported symbols
boundFun :: (MonadReader Ctx m) => Ident -> m (Maybe (Boundedness, ExplicitName))
boundFun name = views localDefinitions (fmap (Function,) <$> Map.lookup name)


-- | Returns the expanded namespace of the symbol
boundImported ::
    (MonadState Env m) => Namespace -> Ident -> m (Maybe (Boundedness, ExplicitName))
boundImported namespace name = do
    let name' = EName namespace name name
    exists <- uses importedDefinitions (Set.member name')
    if exists then pure $ Just (Imported, name') else pure Nothing


boundCons :: (MonadState Env m) => Ident -> m (Maybe (Boundedness, ExplicitName))
boundCons name = uses constructors (fmap (Constructor,) <$> Map.lookup name)


boundArg ::
    (MonadState Env m, MonadReader Ctx m) => Ident -> m (Maybe (Boundedness, ExplicitName))
boundArg name = do
    (close :| rest) <- use arguments
    case Map.lookup name close of
        Just name -> pure (Just (Bound, name))
        Nothing -> pure ((Free,) <$> findArg name rest)
  where
    findArg :: Ident -> [Map Ident ExplicitName] -> Maybe ExplicitName
    findArg _ [] = Nothing
    findArg name (x : xs) = case Map.lookup name x of
        Just info -> pure info
        Nothing -> findArg name xs


lookupBuiltin :: (MonadReader Ctx m) => Namespace -> Ident -> m (Maybe (Boundedness, ExplicitName))
lookupBuiltin namespace name = do
    map <- view builtins
    let name' = EName namespace name name
    if isBuiltin name' map
        then pure (Just (Builtin, name'))
        else pure Nothing


doesNamespaceExist :: (MonadReader Ctx m) => Namespace -> m Bool
doesNamespaceExist namespace = views namespaces (Set.member namespace)


{-| Checks if a variable is bound in the closest scope
  | It does *not* check if a variable is completely unbound
-}
boundVar ::
    (MonadState Env m, MonadReader Ctx m) => Ident -> m (Maybe (Boundedness, ExplicitName))
boundVar name = do
    (close :| rest) <- use scope
    case Map.lookup name close of
        Just name -> pure $ Just (Bound, name)
        Nothing -> pure ((Free,) <$> findVar name rest)
  where
    findVar :: Ident -> [Map Ident ExplicitName] -> Maybe ExplicitName
    findVar _ [] = Nothing
    findVar name (x : xs) = case Map.lookup name x of
        Just name' -> pure name'
        Nothing -> findVar name xs


-- | Insert and rename a variable into the outermost scope
insertVar :: (MonadReader Ctx m, MonadState Env m) => Ident -> m ExplicitName
insertVar name@(Ident nm) = do
    outer <- uses scope head
    numb <- use numbering
    namespace <- view namespace
    let n = Map.findWithDefault 0 name numb + 1
    let name' = EName namespace (Ident $ nm <> "$" <> show n) name
    let outer' = Map.insert name name' outer
    modifying scope (outer' <|)
    modifying numbering (Map.insert name n)
    pure name'


insertArg :: (MonadState Env m) => Namespace -> Ident -> m ExplicitName
insertArg namespace name@(Ident nm) = do
    numb <- use numbering
    let n = Map.findWithDefault 0 name numb + 1
    let name' = EName namespace (Ident $ nm <> "$" <> show n) name
    modifying numbering (Map.insert name n)
    (close :| rest) <- use arguments
    let args = Map.insert name name' close :| rest
    assign arguments args
    pure name'


insertImportName :: (MonadState Env m) => Ident -> Namespace -> m ()
insertImportName name path = modifying importName (Map.insert name path)


resetArgs :: (MonadState Env m) => m ()
resetArgs = assign arguments (mempty :| [])


checkAndinsertConstrutor ::
    (MonadValidate [RnError] m, MonadState Env m, MonadReader Ctx m) => SourceInfo -> Ident -> m ()
checkAndinsertConstrutor loc name = do
    uses constructors (Map.lookup name) >>= \case
        Just name -> conflictingDefinitionArgument loc name
        Nothing -> do
            namespace <- view namespace
            -- FIXME: This might be incorrect
            modifying constructors (Map.insert name (EName namespace name name))


newContext :: Gen a -> Gen a
newContext rn = do
    before <- use scope
    modifying scope (Map.empty <|)
    res <- rn
    assign scope before
    pure res
