{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Error where

import Control.Monad.Validate
import Data.Text (intercalate, pack)
import Frontend.Renamer.Types (ExprRn)
import Frontend.TH
import Frontend.Typechecker.Types
import Relude hiding (All, First, intercalate)
import Text.Megaparsec (unPos)
import Frontend.Types (BinOp, Ident, SourceInfo (..), Span (..), pPretty)
import Utils (indent, quote)

data RnError
    = UnboundVariable SourceInfo Ident
    | ConflictingDefinitionArgument SourceInfo Ident
    | DuplicateToplevels SourceInfo Ident
    deriving (Show)

data TcError
    = TyExpectedGot SourceInfo ExprRn [TypeTc] TypeTc
    | ImmutableVariable SourceInfo ExprRn Ident
    | EmptyReturnNonUnit SourceInfo ExprRn TypeTc
    | ApplyNonFunction SourceInfo ExprRn TypeTc
    | PartiallyAppliedFunction SourceInfo ExprRn Int Int
    | TooManyArguments SourceInfo ExprRn Int Int
    | InvalidOperatorType SourceInfo ExprRn BinOp TypeTc
    | AssignNonVariable SourceInfo ExprRn Ident
    | MissingElse SourceInfo ExprRn
    | ExpectingImmutable SourceInfo ExprRn
    | NonEmptyBreak SourceInfo ExprRn
    | OnlyImmutable SourceInfo ExprRn MetaTy TypeTc
    deriving (Show)

data ChError
    = BreakOutsideLoop SourceInfo
    | MissingReturn SourceInfo Ident
    | UnreachableStatement SourceInfo
    deriving (Show)

data TcWarning = MakeExpressionBreak SourceInfo ExprTc
    deriving (Show)

class Report a where
    report :: a -> Text

instance (Report a) => Report [a] where
    report xs = intercalate "\n\n" $ fmap report xs

instance Report RnError where
    report = reportRnError

instance Report TcError where
    report = show

instance Report TcWarning where
    report = show

instance Report ChError where
    report = show

reportRnError :: RnError -> Text
reportRnError err = case err of
    UnboundVariable info name ->
        combine
            info
            (unwords ["Variable", quote $ pPretty name, "not in scope"])
    ConflictingDefinitionArgument info name -> combine info (unwords ["Conflicting definitions for", quote $ pPretty name])
    DuplicateToplevels info name -> combine info (unwords ["Definition", quote $ pPretty name, "already declared earlier"])

combine :: SourceInfo -> Text -> Text
combine info msg = mconcat [reportSourceInfo info, ":\n", indent 2 ("* " <> msg)]

reportSourceInfo :: SourceInfo -> Text
reportSourceInfo info = do
    let path = pack info.sourceFile
    let pos = do
            Span (startRow, startCol) (_endRow, _endCol) <- info.spanInfo
            pure $ mconcat [show $ unPos startRow, ":", show $ unPos startCol]
    mconcat [path, ":", fromMaybe "?:?" pos]

$(gen All "TcError")
$(gen All "RnError")
$(gen All "ChError")
