{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Parse where

import Data.Text (pack)
import Parser.Types
import Parser.Utils
import Relude
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Types

pTest :: Text -> IO ()
pTest t = case pProgram "" t of
  Left err -> putTextLn err
  Right prog -> print prog

pProgram :: String -> Text -> Either Text (Program Par)
pProgram file source = case P.parse (Program . return <$> pDef <* P.eof) file source of
  Left err -> Left $ pack $ P.errorBundlePretty err
  Right prog -> Right prog

pDef :: Parser DefPar
pDef = do
  (info, (name, args, ty, expressions)) <- posLexeme $ do
    keyword "def"
    name <- identifier
    args <- parens (commaSep pArg)
    ty <- P.option (UnitX ()) (keyword "->" *> pType)
    expressions <- curlyBrackets (P.many pExpr)
    pure (name, args, ty, expressions)
  pure (Fn info name args ty expressions)

pArg :: Parser ArgPar
pArg = do
  (info, (name, ty)) <- posLexeme $ do
    name <- identifier
    keyword ":"
    ty <- pType
    pure (name, ty)
  pure (ArgX info name ty)

pType :: Parser TypePar
pType = P.choice [pAtom, TyFunX () <$> pAtom <*> pType, parens pType]
 where
  pAtom = P.choice [pInt, pDouble, pChar, pString, pUnit, pTyVar]
  pInt :: Parser TypePar
  pInt = IntX () <$ keyword "int"

  pDouble :: Parser TypePar
  pDouble = DoubleX () <$ keyword "double"

  pChar :: Parser TypePar
  pChar = CharX () <$ keyword "char"

  pString :: Parser TypePar
  pString = StringX () <$ keyword "string"

  pUnit :: Parser TypePar
  pUnit = UnitX () <$ keyword "()"

  pTyVar :: Parser TypePar
  pTyVar = TyVarX () <$> identifier

pExpr :: Parser ExprPar
pExpr =
  P.choice
    [ pVar
    , uncurry LitX <$> posLexeme pLit
    ]
 where
  pVar :: Parser ExprPar
  pVar = do
    (posInfo, name) <- posLexeme identifier
    pure (VarX posInfo name)

pLit :: Parser LitPar
pLit = P.choice [pBool, pNumber, pChar, pString]
 where
  pBool :: Parser LitPar
  pBool = BoolLitX () <$> (True <$ keyword "true" <|> False <$ keyword "false")

  pNumber :: Parser LitPar
  pNumber = do
    signed <- P.optional ((negate, negate) <$ char '-' <|> (id, id) <$ P.char '+')
    let (f, g) = fromMaybe (id, id) signed
    DoubleLitX () . f <$> P.float <|> IntLitX () . g <$> P.decimal

  pString :: Parser LitPar
  pString = StringLitX () <$> stringLiteral

  pChar :: Parser LitPar
  pChar = CharLitX () <$> P.charLiteral
