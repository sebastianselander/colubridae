{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Error where

import Control.Monad.Validate (MonadValidate (dispute))
import Data.Text qualified as Text
import Error.Diagnose qualified as Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (hints))
import Frontend.Renamer.Pretty ()
import Frontend.Renamer.Types (PatternRn)
import Frontend.Typechecker.Pretty (pThing)
import Frontend.Typechecker.Types
import Names (Ident (Ident))
import Relude hiding (All, First, intercalate)
import Text.Megaparsec.Error qualified as P
import Utils (quote)

data Error
    = Keyword Text
    | WildCardName
    | UnboundVariable Ident
    | MoreInfo Text
    | ConflictingDefinitionArgument Ident
    | DuplicateToplevels Ident
    | TyExpectedGot [TypeTc] TypeTc
    | ImmutableVariable Ident
    | EmptyReturnNonUnit TypeTc
    | ApplyNonFunction TypeTc
    | PartiallyAppliedFunction Int Int
    | TooManyArguments Int Int
    | AssignNonVariable Ident
    | ExpectingImmutable
    | TypeMustBeKnown Ident
    | ExpectedTyGotLambda TypeTc
    | ExpectedLambdaNArgs Int Int
    | ExpectedPatNArgs PatternRn Int Int
    | BreakOutsideLoop
    | MissingReturn Ident
    | UnreachableStatement
    deriving (Show, Eq, Ord)

instance P.ShowErrorComponent Error where
    showErrorComponent = \case
        Keyword word -> "'" <> Text.unpack word <> "' is a keyword"
        WildCardName -> "Can not use '_' as a variable name"

instance HasHints Error Text where
    hints = \case
        WildCardName -> ["Rename the identifier"]
        Keyword keyword -> ["Add underscore at the end"]

reportError :: Error -> Text
reportError err = case err of
    UnboundVariable (Ident name) -> "cannot find variable `" <> name <> "` in this scope"
    ConflictingDefinitionArgument (Ident name) -> "identifier `" <> name <> "` is bound more than once in here"
    MoreInfo info -> info
    TyExpectedGot expecteds got ->
        Text.unwords
            [ "Expected type"
            , Text.intercalate ", " $ fmap (quote . pThing) expecteds
            , "but got"
            , quote (pThing got)
            ]

newtype TcWarning = MakeExpressionBreak ExprTc
    deriving (Show)

reportSimple ::
    (MonadValidate [Diagnose.Report msg] m) => msg -> [(Diagnose.Position, Diagnose.Marker msg)] -> m ()
reportSimple msg markers = report msg markers []

report ::
    (MonadValidate [Diagnose.Report msg] m) =>
    msg ->
    [(Diagnose.Position, Diagnose.Marker msg)] ->
    [Diagnose.Note msg] ->
    m ()
report msg markers hints = dispute [Diagnose.Err Nothing msg markers hints]
