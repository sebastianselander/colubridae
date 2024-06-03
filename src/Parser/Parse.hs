{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Parse where

import Control.Monad.Combinators.Expr (Operator (..))
import Control.Monad.Combinators.Expr qualified as P
import Data.Text (pack)
import Parser.Types
import Parser.Utils
import Relude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Types

pTest :: Text -> IO ()
pTest t = case pProgram "" t of
  Left err -> putTextLn err
  Right prog -> putTextLn $ pPretty prog

pProgram :: String -> Text -> Either Text (ProgramX Par)
pProgram file source =
  case P.parse (ProgramX () <$> P.many pDef <* P.eof) file source of
    Left err -> Left $ pack $ P.errorBundlePretty err
    Right prog -> Right prog

pDef :: Parser DefPar
pDef = do
  (info, (name, args, ty, expressions)) <- posLexeme $ do
    keyword "def"
    name <- identifier
    args <- parens (commaSep pArg)
    ty <- P.option (UnitX ()) (keyword "->" *> pType)
    expressions <- curlyBrackets (P.many pStmtColon)
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

pStmtColon :: Parser StmtPar
pStmtColon =
  P.choice
    [ pIf
    , pWhile
    , pRet <* semicolon
    , pBreak <* semicolon
    , pLet <* semicolon
    , pBlock
    , pSExp <* semicolon
    , pEmpty <* semicolon
    ]

pStmt :: Parser StmtPar
pStmt =
  P.choice
    [ pIf
    , pWhile
    , pRet
    , pBreak
    , pLet
    , pBlock
    ]

pEmpty :: Parser StmtPar
pEmpty = return (StmtX ())

pIf :: Parser StmtPar
pIf = do
  keyword "if"
  cond <- pExpr
  thenB <- curlyBrackets (P.many pStmtColon)
  elseB <- P.optional (keyword "else" *> curlyBrackets (P.many pStmtColon))
  pure (IfX () cond thenB elseB)

pWhile :: Parser StmtPar
pWhile = do
  keyword "while"
  cond <- pExpr
  loopBody <- curlyBrackets (P.many pStmtColon)
  pure (WhileX () cond loopBody)

pRet :: Parser StmtPar
pRet = do
  keyword "return"
  expr <- P.optional pExpr
  pure (RetX () expr)

pBreak :: Parser StmtPar
pBreak = do
  keyword "break"
  expr <- P.optional pExpr
  pure (RetX () expr)

pLet :: Parser StmtPar
pLet = do
  keyword "let"
  mut <- maybe Immutable (const Mutable) <$> P.optional (keyword "mut")
  name <- identifier
  keyword "="
  LetX mut name <$> pExpr

pBlock :: Parser StmtPar
pBlock = BlockX () <$> curlyBrackets (P.many pStmtColon)

pSExp :: Parser StmtPar
pSExp = SExpX () <$> pExpr

pExpr :: Parser ExprPar
pExpr = putInfo (P.makeExprParser pExprAtom table)
 where
  table =
    [
      [ binary "*" (binOp Mul)
      , binary "/" (binOp Div)
      , binary "%" (binOp Mod)
      ]
    ,
      [ binary "+" (binOp Add)
      , binary "-" (binOp Sub)
      ]
    ,
      [ binary "<=" (binOp Lte)
      , binary ">=" (binOp Gte)
      , binary "<" (binOp Lt)
      , binary ">" (binOp Gt)
      ]
    ,
      [ binary "==" (binOp Eq)
      , binary "!=" (binOp Neq)
      ]
    ,
      [ binary "&&" (binOp And)
      ]
    ,
      [ binary "||" (binOp Or)
      ]
    ,
      [ binary "=" (binOp Assign)
      , binary "+=" (binOp AddAssign)
      , binary "*=" (binOp MulAssign)
      , binary "/=" (binOp DivAssign)
      , binary "%=" (binOp ModAssign)
      ]
    ]

  binary name f = InfixL (f <$ keyword name)
  binOp op l = BinOpX (error "ugly") l op

pExprAtom :: Parser ExprPar
pExprAtom =
  P.choice
    -- [ uncurry LitX <$> posLexeme pLit
    [ uncurry EStmtX <$> pos pStmt
    , pVar
    , parens pExpr
    ]
 where
  pVar :: Parser ExprPar
  pVar = do
    (posInfo, name) <- posLexeme identifier
    pure (VarX posInfo name)

pLit :: Parser LitPar
pLit =
  P.choice
    [ pBool
    , pNumber
    , pChar
    , pString
    ]
 where
  pBool :: Parser LitPar
  pBool = BoolLitX () <$> (True <$ keyword "true" <|> False <$ keyword "false")

  pNumber :: Parser LitPar
  pNumber = DoubleLitX () <$> P.try P.float <|> IntLitX () <$> P.decimal

  pString :: Parser LitPar
  pString = StringLitX () <$> stringLiteral

  pChar :: Parser LitPar
  pChar = CharLitX () <$> charLiteral

putInfo :: Parser ExprPar -> Parser ExprPar
putInfo p = do
  (pos, p) <- pos p
  pure $ case p of
    LitX _ l -> LitX pos l
    VarX _ v -> VarX pos v
    BinOpX _ l op r -> BinOpX pos l op r
    EStmtX _ s -> EStmtX pos s
    AppX _ l rs -> AppX pos l rs
