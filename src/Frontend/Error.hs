module Frontend.Error where

import Types (Ident, SourceInfo)
import Relude

data RnError
  = UnboundVariable SourceInfo Ident
  | ConflictingDefinitionArgument SourceInfo Ident
  | DuplicateToplevels SourceInfo Ident
  deriving (Show)

data TcError
  deriving (Show)

class Report a where
  report :: a -> Text

instance Report RnError where
    report = show

instance Report TcError where
    report = show
