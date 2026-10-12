{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Renamer.Monad
    ( Env,
      Ctx,
      Gen,
      SymbolTable (..),
      lookupFunction,
      lookupType,
      lookupConstructor,
      newContext,
      boundVar,
      insertVar,
      boundArg,
      lookupBuiltin,
      createCtx,
      emptyEnv,
      freeVarsAllowed,
      importName,
      symbols,
      namespace,
      numbering,
      runGen,
      insertArg,
      arguments,
      resetArgs,
      insertImportName,
    )
where

import Control.Lens hiding ((<|))
import Control.Monad.Validate (MonadValidate, Validate, runValidate)
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Relude hiding (Map, head)

import Data.Map.Strict qualified as Map

import Frontend.Builtin (Builtins, isBuiltin)
import Frontend.Error
import Frontend.Typechecker.Types (Tc)
import Frontend.Types (Boundedness (..))
import Names (ExplicitName (EName), Ident (..), Namespace)


data Env = Env
    { _numbering :: Map Ident Int
    , _scope :: NonEmpty (Map Ident ExplicitName)
    , _arguments :: NonEmpty (Map Ident ExplicitName)
    , _importName :: Map Ident Namespace -- as-name to import name (path)
    }
    deriving (Show)


data SymbolTable = SymbolTable
    { functions :: Map Namespace (Map Ident ExplicitName)
    -- ^ namespace -> function -> name
    , datatypes :: Map Namespace (Map Ident ExplicitName)
    -- ^ namespace -> data type -> name
    , constructors :: Map Namespace (Map Ident ExplicitName)
    {- ^ namespace -> constructor -> name
    note: Here namespace will be the concatenation of the file namespace and the type namespace
    example: in foo.ck we have type List { Nil}: perform the lookup on foo::List
    -}
    }
    deriving (Show)


data Ctx = Ctx
    { _freeVarsAllowed :: Bool
    , _symbols :: SymbolTable
    , _namespace :: Namespace
    , _builtins :: Builtins Tc
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


emptyEnv :: Env
emptyEnv =
    Env
        { _numbering = mempty
        , _scope = return mempty
        , _arguments = mempty :| []
        , _importName = mempty
        }


createCtx :: SymbolTable -> Namespace -> Builtins Tc -> Ctx
createCtx = Ctx True


runGen :: Env -> Ctx -> Gen a -> Either [RnError] a
runGen env ctx =
    runValidate
        . flip runReaderT ctx
        . flip evalStateT env
        . runGen'


lookupFunction ::
    (MonadReader Ctx m) => Namespace -> Ident -> m (Maybe ExplicitName)
lookupFunction namespace name =
    views symbols (Map.lookup name <=< Map.lookup namespace . functions)


lookupType ::
    (MonadReader Ctx m) => Namespace -> Ident -> m (Maybe ExplicitName)
lookupType namespace name = views symbols (Map.lookup name <=< Map.lookup namespace . datatypes)


lookupConstructor ::
    (MonadReader Ctx m) => Namespace -> Ident -> m (Maybe ExplicitName)
lookupConstructor namespace name =
    views
        symbols
        (Map.lookup name <=< Map.lookup namespace . constructors)


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


newContext :: Gen a -> Gen a
newContext rn = do
    before <- use scope
    modifying scope (Map.empty <|)
    res <- rn
    assign scope before
    pure res
