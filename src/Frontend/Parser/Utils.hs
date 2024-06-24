{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Frontend.Parser.Utils where

import Data.Text (pack)
import Relude hiding (span)
import Text.Megaparsec (Pos)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Types
import Text.Megaparsec.Byte.Lexer (skipLineComment)

type Parser = P.ParsecT CustomParseError Text (Reader Bool)

{-| The order of the errors matters here, the one with the 'greatest' ord
takes priority if more than one error is thrown
-}
data CustomParseError = BreakNotInLoop
    deriving (Eq, Ord, Show)

instance P.ShowErrorComponent CustomParseError where
    showErrorComponent = show

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
parens = lexeme . P.between (P.char '(') (P.char ')')

angles :: Parser a -> Parser a
angles = lexeme . P.between (P.char '<') (P.char '>')

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
curlyBrackets = P.between (lexeme $ P.char '{') (P.char '}')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (P.hidden P.space) --L.lexeme (void $ P.many $ P.hidden (P.space <|> L.skipLineComment "//" <|> L.skipBlockCommentNested "/*" "*/"))

string :: Text -> Parser Text
string txt = lexeme (P.string txt)

commaSep :: Parser a -> Parser [a]
commaSep p = P.sepBy p (lexeme $ P.char ',')

stringLiteral :: Parser Text
stringLiteral = P.char '"' >> pack <$> P.manyTill L.charLiteral (P.char '"')

charLiteral :: Parser Char
charLiteral = P.between (P.char '\'') (P.char '\'') L.charLiteral

identifier :: Parser Ident
identifier = do
    headLet <- P.char '_' <|> P.letterChar
    tailLets <- many (P.char '_' <|> P.alphaNumChar)
    let name = headLet : tailLets
    if name `elem` keywords
        then fail $ "'" <> name <> "' is a keyword, you can not use it as an identifer"
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
