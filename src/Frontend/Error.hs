{-# LANGUAGE TemplateHaskell #-}

module Frontend.Error where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Validate
import Frontend.TH
import Frontend.Typechecker.Types (MetaTy (..), TypeTc)
import Relude hiding (All, First)
import Types (Ident, SourceInfo, TypeX (..))

data RnError
    = UnboundVariable SourceInfo Ident
    | ConflictingDefinitionArgument SourceInfo Ident
    | DuplicateToplevels SourceInfo Ident
    deriving (Show)

data TcError
    = TyExpectedGot SourceInfo [TypeTc] TypeTc
    | EmptyReturnNonUnit SourceInfo TypeTc
    deriving (Show)

class Report a where
    report :: a -> Text

instance Report RnError where
    report = show

instance Report TcError where
    report = show

doneTcError :: (MonadValidate [TcError] m) => m a
doneTcError = refute []

$(gen All "TcError")
$(gen First "RnError")
