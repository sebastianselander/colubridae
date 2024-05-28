{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parse where

import Control.Monad.Combinators.Expr
import Data.Text (pack)
import Generics.SYB (Data, gmapT, mkT)
import Language
import Relude
import Text.Megaparsec (getSourcePos)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

-- TODO: Save position

type Parser a = P.Parsec Void Text a

keywords :: [String]
keywords = ["let", "in", "="]

keyword :: Text -> Parser Ident
keyword = fmap Ident . lexeme . P.string

parse :: Text -> IO ()
parse input = case P.parse (pExpr <* P.eof) "" input of
    Right res -> putStrLn (pPrint res)
    Left err -> putStrLn $ P.errorBundlePretty err

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
    let name = headLet : tailLets
    if name `elem` keywords then fail $ "can't parse keyword '" <> name <> "' as identifer" else return (pack $ headLet : tailLets)

pExpr :: Parser SynExpr
pExpr = exprTable

pAtom :: Parser SynExpr
pAtom = P.choice [pInt, pLam, pLet, pVar, parensExpr]

pIdent :: Parser Ident
pIdent = Ident <$> lexeme identifier

pVar :: Parser SynExpr
pVar = putInfo $ Var safe <$> P.try pIdent

pInt :: Parser SynExpr
pInt = putInfo $ Lit safe <$> lexeme L.decimal

pLet :: Parser SynExpr
pLet = putInfo $ do
    keyword "let"
    name <- pIdent
    keyword "="
    e1 <- pExpr
    keyword "in"
    Let safe name e1 <$> pExpr

pLam :: Parser SynExpr
pLam = putInfo $ Lam safe <$> (char '\\' *> pIdent <* string "->") <*> exprTable

parensExpr :: Parser SynExpr
parensExpr = putInfo $ parens exprTable

exprTable :: Parser SynExpr
exprTable = putInfo $ makeExprParser pAtom opTable
  where
    opTable =
        [ [InfixL (char '*' $> Mul safe)]
        , [InfixL (char '+' $> Plus safe)]
        , [InfixL (return () $> App safe)]
        ]

putInfo :: Parser SynExpr -> Parser SynExpr
putInfo p = do
    posStart <- getSourcePos
    expr <- p
    posEnd <- getSourcePos
    let info = SynInfo{posStart, posEnd}
    pure (shallow (const info) expr)

shallow :: (Data a, Typeable b) => (b -> b) -> a -> a
shallow f = gmapT (mkT f)

safe :: a
safe = error "This will never be evaluted thanks to laziness"
