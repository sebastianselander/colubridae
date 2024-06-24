{-# LANGUAGE TemplateHaskell #-}

module Frontend.Error where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Validate
import Frontend.TH
import Frontend.Typechecker.Types
import Relude hiding (All, First)
import Types (Ident, SourceInfo, TypeX (..), BinOp)

data RnError
    = UnboundVariable SourceInfo Ident
    | ConflictingDefinitionArgument SourceInfo Ident
    | DuplicateToplevels SourceInfo Ident
    deriving (Show)

data TcError
    = TyExpectedGot SourceInfo [TypeTc] TypeTc
    | ImmutableVariable SourceInfo Ident
    | EmptyReturnNonUnit SourceInfo TypeTc
    | ApplyNonFunction SourceInfo TypeTc
    | PartiallyAppliedFunction SourceInfo Int Int
    | TooManyArguments SourceInfo Int Int
    | InvalidOperatorType SourceInfo BinOp TypeTc
    | AssignNonVariable SourceInfo Ident
    | MissingElse SourceInfo
    | ExpectingImmutable SourceInfo
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
