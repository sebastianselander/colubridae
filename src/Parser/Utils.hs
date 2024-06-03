module Parser.Utils where

import Data.Text (pack)
import Relude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos (SourcePos)
import Types

type Parser a = P.Parsec Void Text a

data SourceInfo = SourceInfo
  { before :: SourcePos
  , after :: SourcePos
  }
  deriving (Show, Eq, Ord)

keywords :: [String]
keywords = [ "def" ]

keyword :: Text -> Parser ()
keyword = void . lexeme . P.string

parens :: Parser a -> Parser a
parens = lexeme . P.between (char '(') (char ')')

curlyBrackets :: Parser a -> Parser a
curlyBrackets = lexeme . P.between (char '{') (char '}')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme P.space

char :: Char -> Parser Char
char c = lexeme (P.char c)

string :: Text -> Parser Text
string txt = lexeme (P.string txt)

commaSep :: Parser a -> Parser [a]
commaSep p = P.sepBy p (lexeme $ char ',')

stringLiteral :: Parser Text
stringLiteral = char '"' >> pack <$> P.manyTill L.charLiteral (char '"')

identifier :: Parser Ident
identifier = do
  headLet <- P.char '_' <|> P.letterChar
  tailLets <- many (P.char '_' <|> P.alphaNumChar)
  let name = headLet : tailLets
  if name `elem` keywords
    then fail $ "can't parse keyword '" <> name <> "' as identifer"
    else pure (Ident (pack (headLet : tailLets)))

posLexeme :: Parser a -> Parser (SourceInfo, a)
posLexeme p = lexeme (pos p)

pos :: Parser a -> Parser (SourceInfo, a)
pos p = do
  before <- P.getSourcePos
  res <- p
  after <- P.getSourcePos
  pure (SourceInfo {before, after}, res)
