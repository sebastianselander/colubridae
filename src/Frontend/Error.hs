{-# LANGUAGE TemplateHaskell #-}

module Frontend.Error where

import Types (Ident, SourceInfo, TypeX (..))
import Relude hiding (First, All)
import Frontend.Typechecker.Types (TypeTc, MetaTy (..))
import Frontend.TH
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Validate

data RnError
  = UnboundVariable SourceInfo Ident
  | ConflictingDefinitionArgument SourceInfo Ident
  | DuplicateToplevels SourceInfo Ident
  deriving (Show)

data TcError 
  = TyExpectedGot SourceInfo [TypeTc] TypeTc
  deriving (Show)

class Report a where
  report :: a -> Text

instance Report RnError where
    report = show

instance Report TcError where
    report = show

doneTcError :: MonadValidate [TcError] m => m a
doneTcError = refute []

$(gen All "TcError")
$(gen First "RnError")
