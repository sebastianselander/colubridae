{-# LANGUAGE TemplateHaskell #-}
module Renamer.Error where

import GHC.Show (Show)
import Types (Ident, SourceInfo)
import Renamer.TH
import Relude
import Data.Either.Extra (mapLeft)

data RnError
  = UnboundVariable SourceInfo Ident
  | ConflictingDefinitionArgument SourceInfo Ident
  deriving (Show)

pure []

class Report a where
  report :: a -> Text

instance Report RnError where
    report = show

onErr :: Either RnError a -> Either Text a
onErr = mapLeft report
