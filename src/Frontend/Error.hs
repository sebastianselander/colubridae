{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Error where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Validate
import Data.Text (intercalate)
import Frontend.TH
import Frontend.Typechecker.Types
import Relude hiding (All, First, intercalate)
import Types (BinOp, Ident, SourceInfo)
import Frontend.Renamer.Types (ExprRn)

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
    report = show

instance Report TcError where
    report = show

instance Report TcWarning where
    report = show

instance Report ChError where
    report = show

reportRnError :: RnError -> Text
reportRnError err = case err of
    UnboundVariable info name -> "unbound variable: " <> show name
    ConflictingDefinitionArgument {} -> undefined
    DuplicateToplevels {} -> undefined

$(gen All "TcError")
$(gen First "RnError")
$(gen All "ChError")
