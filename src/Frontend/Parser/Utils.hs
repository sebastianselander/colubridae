{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Parser.Utils where

import Data.Text (pack)
import Relude hiding (span)
import Text.Megaparsec (Pos, ParseErrorBundle, customFailure, (<?>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Frontend.Types
import Frontend.Error (Report, report)
import Text.Megaparsec.Error (errorBundlePretty)
import Names (Ident(..))

type Parser = P.Parsec CustomParseError Text

{-| The order of the errors matters here, the one with the 'greatest' ord
takes priority if more than one error is thrown *I think*
-}
data CustomParseError = UppercaseLetter | Keyword Text
    deriving (Eq, Ord, Show)

instance P.ShowErrorComponent CustomParseError where
    showErrorComponent = show

instance Report (ParseErrorBundle Text CustomParseError) where
    report = pack . errorBundlePretty


keywords :: [String]
keywords =
    [ "def"
    , "="
    , "let"
    , "mut"
    , "while"
    , "break"
    , "return"
    , "if"
    , "true"
    , "false"
    , "int"
    , "bool"
    , "double"
    , "string"
    , "char"
    , "fn"
    , "{"
    , "}"
    , "("
    , ")"
    , "["
    , "]"
    ]

keyword :: Text -> Parser ()
keyword = void . P.string

parens :: Parser a -> Parser a
parens = lexeme . P.between (P.hidden $ P.char '(') (P.hidden $ P.char ')')

angles :: Parser a -> Parser a
angles = lexeme . P.between (P.hidden $ P.char '<') (P.hidden $ P.char '>')

semicolon :: Parser Char
semicolon = P.char ';'

optionallyEndedBy :: (P.MonadParsec e s m) => m a -> m end -> m ([a], Maybe end)
optionallyEndedBy aP endP = do
    P.optional (P.try endP) >>= \case
        Nothing -> do
            a <- aP
            first (a :) <$> (optionallyEndedBy aP endP <|> pure ([], Nothing))
        Just res -> do
            pure ([], Just res)

curlyBrackets :: Parser a -> Parser a
curlyBrackets = P.between (lexeme $ P.hidden $ P.char '{') (P.hidden $ P.char '}')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (P.hidden P.space) --L.lexeme (void $ P.many $ P.hidden (P.space <|> L.skipLineComment "//" <|> L.skipBlockCommentNested "/*" "*/"))

string :: Text -> Parser Text
string txt = lexeme (P.string txt)

commaSep :: Parser a -> Parser [a]
commaSep p = P.sepBy p (P.hidden $ lexeme $ P.char ',')

stringLiteral :: Parser Text
stringLiteral = P.hidden (P.char '"') >> pack <$> P.manyTill L.charLiteral (P.hidden (P.char '"'))

charLiteral :: Parser Char
charLiteral = P.between (P.hidden $ P.char '\'') (P.hidden $ P.char '\'') L.charLiteral

identifier :: Parser Ident
identifier = do
    headLet <- P.char '_' <|> (P.lowerChar <?> "lower case identifier")
    tailLets <- many (P.char '_' <|> P.alphaNumChar)
    let name = headLet : tailLets
    if name `elem` keywords
        then customFailure (Keyword (pack name))
        else pure (Ident (pack (headLet : tailLets)))

data Before

newtype GhostSpan a = GS (Pos, Pos)

spanStart :: Parser (GhostSpan Before)
spanStart = do
    pos <- P.getSourcePos
    pure $ GS (pos.sourceLine, pos.sourceColumn)

spanEnd :: GhostSpan Before -> Parser SourceInfo
spanEnd (GS before) = do
    after <- P.getSourcePos
    let span = Span {start = before, end = (after.sourceLine, after.sourceColumn)}
    let info = SourceInfo {sourceFile = after.sourceName, spanInfo = Just span}
    lexeme (return ())
    pure info

span :: GhostSpan Before -> Parser a -> Parser (a, SourceInfo)
span gs p = do
    res <- p
    info <- spanEnd gs
    pure (res, info)

emptyInfo :: SourceInfo
emptyInfo = SourceInfo {sourceFile = "", spanInfo = Nothing}
