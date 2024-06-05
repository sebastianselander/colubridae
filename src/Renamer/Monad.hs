{-# LANGUAGE TemplateHaskell #-}
module Renamer.Monad where

import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Relude hiding (Map)
import Renamer.Error (RnError(..))
import Types (Ident (..))
import Renamer.TH

data Env = Env
  { conversionKV :: Map Ident Ident -- Just for remembering
  , numbering :: Map Ident Int
  , scope :: NonEmpty (Map Ident Ident)
  }
  deriving (Show)

data Ctx = Ctx {definitions :: Set Ident, closestScope :: Set Ident}
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
emptyCtx = Ctx mempty mempty

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
bound :: Ident -> Gen Bool
bound name = asks (Set.member name . closestScope)

context :: Gen a -> Gen a
context rn = do
  before <- get
  res <- rn
  put before
  pure res

findIdent :: Ident -> Gen (Maybe Ident)
findIdent name = find <$> gets (toList . scope)
 where
  find :: [Map Ident Ident] -> Maybe Ident
  find [] = Nothing
  find (x : xs) = Map.lookup name x <|> find xs

isVariable :: Ident -> Gen Bool
isVariable name = isJust <$> findIdent name

isDefinition :: Ident -> Gen Bool
isDefinition name = asks (Set.member name . definitions)

$(gen "RnError")
