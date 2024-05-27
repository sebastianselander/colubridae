{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parse where

import Data.Text (pack)
import Relude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Control.Monad.Combinators.Expr
import Language
import Text.Megaparsec.Char.Lexer qualified as L

type Parser a = P.Parsec Void Text a

parse :: Text -> Either (P.ParseErrorBundle Text Void) Expr
parse = P.parse exprTable ""

parens :: Parser a -> Parser a
parens = P.between (char '(') (char ')')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme P.space

char :: Char -> Parser Char
char c = lexeme (P.char c)

string :: Text -> Parser Text
string txt = lexeme (P.string txt)

identifier :: Parser Text
identifier = do
    headLet <- P.char '_' <|> P.letterChar
    tailLets <- many (P.char '_' <|> P.alphaNumChar)
    return $ pack (headLet : tailLets)

pVar :: Parser Ident
pVar = Ident <$> lexeme identifier

pInt :: Parser Expr
pInt = Lit <$> lexeme L.decimal

pAtom :: Parser Expr
pAtom = P.choice [pInt, Var <$> pVar, pLam, parens exprTable]

pLam :: Parser Expr
pLam = Lam <$> (char '\\' *> pVar <* string "->") <*> exprTable

pApp :: Parser (Expr -> Expr -> Expr)
pApp = return () $> App

exprTable :: Parser Expr
exprTable = makeExprParser pAtom opTable
  where
    opTable = [ [InfixL pApp] ]
