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
      emptyCtx,
      emptyEnv,
      definitions,
      numbering,
      newToOld,
      runGen,
      boundFun,
      insertArg,
      names,
      checkAndinsertConstrutor,
      arguments,
      resetArgs,
    ) where

import Control.Lens hiding ((<|))
import Control.Monad.Validate (MonadValidate, Validate, runValidate)
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Frontend.Builtin (builtInNames)
import Frontend.Error
import Frontend.Renamer.Types (Boundedness (..))
import Frontend.Types (SourceInfo)
import Names (Ident (..))
import Relude hiding (Map, head)

data Env = Env
    { _newToOld :: Map Ident Ident
    , _numbering :: Map Ident Int
    , _scope :: NonEmpty (Map Ident Ident)
    , _arguments :: Map Ident Ident
    , _constructors :: Set Ident
    }
    deriving (Show)

newtype Ctx = Ctx {_definitions :: Set Ident}
    deriving (Show)

$(makeLenses ''Env)
$(makeLenses ''Ctx)

newtype Gen a = Gen {runGen' :: StateT Env (ReaderT Ctx (Validate [RnError])) a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState Env
        , MonadReader Ctx
        , MonadValidate [RnError]
        )

emptyEnv :: Env
emptyEnv = Env mempty mempty (return mempty) mempty mempty

emptyCtx :: Ctx
emptyCtx = Ctx builtInNames

runGen :: Env -> Ctx -> Gen a -> Either [RnError] a
runGen env ctx =
    runValidate
        . flip runReaderT ctx
        . flip evalStateT env
        . runGen'

names :: Gen (Map Ident Ident)
names = use newToOld

boundFun :: (MonadReader Ctx m) => Ident -> m (Maybe Ident)
boundFun name = views definitions (bool Nothing (Just name) . Set.member name)

boundCons :: (MonadState Env m) => Ident -> m (Maybe Ident)
boundCons name = uses constructors (bool Nothing (Just name) . Set.member name)

boundArg :: (MonadState Env m) => Ident -> m (Maybe Ident)
boundArg name = uses arguments (Map.lookup name)

{-| Checks if a variable is bound in the closest scope
  | It does *not* check if a variable is completely unbound
-}
boundVar :: (MonadState Env m) => Ident -> m (Maybe (Boundedness, Ident))
boundVar name = do
    (close :| rest) <- use scope
    case Map.lookup name close of
        Just name' -> pure $ Just (Bound, name')
        Nothing -> pure ((Free,) <$> findVar name rest)
  where
    findVar :: Ident -> [Map Ident Ident] -> Maybe Ident
    findVar _ [] = Nothing
    findVar name (x : xs) = case Map.lookup name x of
        Just name' -> pure name'
        Nothing -> findVar name xs

-- | Insert and rename a variable into the outermost scope
insertVar :: (MonadState Env m) => Ident -> m Ident
insertVar name@(Ident nm) = do
    outer <- uses scope head
    numb <- use numbering
    let n = Map.findWithDefault 0 name numb + 1
    let name' = Ident $ nm <> "$" <> show n
    let outer' = Map.insert name name' outer
    modifying newToOld (Map.insert name' name)
    modifying scope (outer' <|)
    modifying numbering (Map.insert name n)
    pure name'

insertArg :: (MonadState Env m) => Ident -> m Ident
insertArg name@(Ident nm) = do
    numb <- use numbering
    let n = Map.findWithDefault 0 name numb + 1
    let name' = Ident $ nm <> "$" <> show n
    modifying newToOld (Map.insert name' name)
    modifying numbering (Map.insert name n)
    modifying arguments (Map.insert name name')
    pure name'

resetArgs :: MonadState Env m => m ()
resetArgs = modifying arguments mempty

checkAndinsertConstrutor ::
    (MonadValidate [RnError] m, MonadState Env m) => SourceInfo -> Ident -> m ()
checkAndinsertConstrutor loc name = do
    uses constructors (Set.member name) >>= \case
        True -> conflictingDefinitionArgument loc name
        False -> modifying constructors (Set.insert name)

newContext :: Gen a -> Gen a
newContext rn = do
    before <- use scope
    modifying scope (Map.empty <|)
    res <- rn
    assign scope before
    pure res
