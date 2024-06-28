{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Renamer.Monad where

import Control.Lens hiding ((<|))
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Frontend.Error
import Frontend.Renamer.Types (Boundedness (..))
import Relude hiding (Map)
import Frontend.Types (Ident (..))
import Control.Monad.Validate (MonadValidate, Validate, runValidate)

data Env = Env
    { _conversionKV :: Map Ident Ident -- Just for remembering
    , _numbering :: Map Ident Int
    , _scope :: NonEmpty (Map Ident Ident)
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
emptyEnv = Env mempty mempty (return mempty)

emptyCtx :: Ctx
emptyCtx = Ctx mempty

runGen :: Env -> Ctx -> Gen a -> Either [RnError] a
runGen env ctx =
    runValidate
        . flip runReaderT ctx
        . flip evalStateT env
        . runGen'

names :: Gen (Map Ident Ident)
names = use conversionKV

conversions :: Gen (Map Ident Ident)
conversions = use conversionKV

fresh :: Ident -> Gen Ident
fresh name = do
    Env {_conversionKV, _numbering, _scope} <- get
    let (n, name') = case Map.lookup name _numbering of
            Nothing -> (1, name <> Ident (show (1 :: Int)))
            Just n -> (n + 1, name <> Ident (show (n + 1)))
    let names' = Map.insert name' name _conversionKV
    let nameCounter = Map.insert name n nameCounter
    put Env {_conversionKV = names', _numbering = nameCounter, _scope}
    pure name'

{-| Checks if a variable is bound in the closest scope
| It does *not* check if a variable is completely unbound
-}
boundFun :: (MonadReader Ctx m) => Ident -> m (Maybe Ident)
boundFun name = views definitions (bool Nothing (Just name) . Set.member name)

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
    (outer :| rest) <- use scope
    numb <- use numbering
    let n = Map.findWithDefault 0 name numb + 1
    let numbering' = Map.insert name n numb
    let name' = Ident $ nm <> "#" <> show n
    let outer' = Map.insert name name' outer
    assign scope (outer' :| rest)
    assign numbering numbering'
    pure name'

newContext :: Gen a -> Gen a
newContext rn = do
    before <- get
    modifying scope (Map.empty <|)
    res <- rn
    put before
    pure res
