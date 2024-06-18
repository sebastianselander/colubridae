{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Renamer.Monad where

import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Relude hiding (Map)
import Frontend.Error (RnError(..))
import Types (Ident (..))
import Frontend.TH
import Data.List.NonEmpty ((<|))
import Frontend.Renamer.Types (Boundedness(..))

data Env = Env
  { conversionKV :: Map Ident Ident -- Just for remembering
  , numbering :: Map Ident Int
  , scope :: NonEmpty (Map Ident Ident)
  }
  deriving (Show)

newtype Ctx = Ctx {definitions :: Set Ident}
  deriving (Show)

newtype Gen a = Gen {runGen' :: StateT Env (ReaderT Ctx (Except RnError)) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Env
    , MonadReader Ctx
    , MonadError RnError
    )

emptyEnv :: Env
emptyEnv = Env mempty mempty (return mempty)

emptyCtx :: Ctx
emptyCtx = Ctx mempty

runGen :: Env -> Ctx -> Gen a -> Either RnError a
runGen env ctx =
  runExcept
    . flip runReaderT ctx
    . flip evalStateT env
    . runGen'

names :: Gen (Map Ident Ident)
names = gets conversionKV

conversions :: Gen (Map Ident Ident)
conversions = gets conversionKV

fresh :: Ident -> Gen Ident
fresh name = do
  Env {conversionKV, numbering, scope} <- get
  let (n, name') = case Map.lookup name numbering of
        Nothing -> (1, name <> Ident (show (1 :: Int)))
        Just n -> (n + 1, name <> Ident (show (n + 1)))
  let names' = Map.insert name' name conversionKV
  let nameCounter = Map.insert name n nameCounter
  put Env {conversionKV = names', numbering = nameCounter, scope}
  pure name'

{-| Checks if a variable is bound in the closest scope
| It does *not* check if a variable is completely unbound
-}

bound :: MonadState Env m => Ident -> m (Maybe (Boundedness, Ident))
bound name = do
    (close :| rest) <- gets scope
    case Map.lookup name close of
        Just name' -> pure $ Just (Bound, name')
        Nothing -> pure ((Free,) <$> findVar name rest)
  where
    findVar :: Ident -> [Map Ident Ident] -> Maybe Ident
    findVar _ [] = Nothing
    findVar name (x:xs) = case Map.lookup name x of
        Just name' -> pure name'
        Nothing -> findVar name xs

-- | Insert and rename a variable into the outermost scope
insertVar :: Ident -> Gen Ident
insertVar name@(Ident nm) = do
    state <- get
    let (outer :| rest) = state.scope
    let n = Map.findWithDefault 0 name state.numbering + 1
    let numbering' = Map.insert name n state.numbering
    let name' = Ident $ nm <> "#" <> show n
    let outer' = Map.insert name name' outer
    put (state { scope = outer' :| rest, numbering = numbering'})
    pure name'

newContext :: Gen a -> Gen a
newContext rn = do
  before <- get
  modify $ \s -> s { scope = mempty <| s.scope }
  res <- rn
  put before
  pure res

$(gen "RnError")
