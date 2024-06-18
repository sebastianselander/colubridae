{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Frontend.Parser.Parse where

import Control.Monad.Combinators.Expr (Operator (..))
import Control.Monad.Combinators.Expr qualified as P
import Data.Text (pack)
import Frontend.Parser.Types
import Frontend.Parser.Utils
import Relude hiding (span)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Types
import Utils (impossible)

pTest :: Text -> IO ()
pTest t = case pProgram "" t of
  Left err -> putTextLn err
  Right prog -> putTextLn $ pPretty prog

pProgram :: String -> Text -> Either Text ProgramPar
pProgram file source =
  case P.parse (ProgramX () <$> P.many pDef <* P.eof) file source of
    Left err -> Left $ pack $ P.errorBundlePretty err
    Right prog -> Right prog

runP :: (Show a) => Parser a -> Text -> IO ()
runP p = P.parseTest (p <* P.eof)

pDef :: Parser DefPar
pDef = do
  gs <- spanStart
  lexeme (keyword "def")
  name <- lexeme identifier
  args <- parens (commaSep pArg)
  ty <- P.option (UnitX ()) (lexeme (keyword "->") *> pType)
  expressions <- curlyBrackets (P.many (lexeme pStmtColon))
  info <- spanEnd gs
  pure (Fn info name args ty expressions)

pArg :: Parser ArgPar
pArg = do
  gs <- spanStart
  mut <- P.option Immutable $ lexeme (keyword "mut") $> Mutable
  name <- identifier
  lexeme (keyword ":")
  (ty, info) <- span gs pType
  pure (ArgX (info, mut) name ty)

pType :: Parser TypePar
pType = P.choice [pAtom, pFunTy, parens pType]
 where
  pFunTy :: Parser TypePar
  pFunTy = TyFunX () <$> pAtom <*> pType

  pAtom :: Parser TypePar
  pAtom = P.choice [pInt, pDouble, pChar, pString, pUnit, pTyVar]

  pInt :: Parser TypePar
  pInt = IntX () <$ lexeme (keyword "int")

  pDouble :: Parser TypePar
  pDouble = DoubleX () <$ lexeme (keyword "double")

  pChar :: Parser TypePar
  pChar = CharX () <$ lexeme (keyword "char")

  pString :: Parser TypePar
  pString = StringX () <$ lexeme (keyword "string")

  pUnit :: Parser TypePar
  pUnit = UnitX () <$ lexeme (keyword "()")

  pTyVar :: Parser TypePar
  pTyVar = TyVarX () <$> identifier

pStmtColon :: Parser StmtPar
pStmtColon = lexeme $
  P.choice
    [ fst <$> pIf
    , fst <$> pWhile
    , fst <$> pRet <* semicolon
    , fst <$> pBreak <* semicolon
    , fst <$> pLet <* semicolon
    , fst <$> pBlock
    , fst <$> pAss <* semicolon
    , fst <$> pSExp <* semicolon
    , pEmpty <* semicolon
    ]

pStmt :: Parser (StmtPar, SourceInfo)
pStmt =
  P.choice
    [ pIf
    , pWhile
    , pRet
    , pBreak
    , pLet
    , pBlock
    , pAss
    ]

pEmpty :: Parser StmtPar
pEmpty = return (StmtX ())

pIf :: Parser (StmtPar, SourceInfo)
pIf = do
  gs <- spanStart
  lexeme (keyword "if")
  cond <- pExpr
  thenB <- curlyBrackets (P.many pStmtColon)
  elseB <- P.optional (lexeme (keyword "else") *> curlyBrackets (P.many pStmtColon))
  info <- spanEnd gs
  pure (IfX () cond thenB elseB, info)

pWhile :: Parser (StmtPar, SourceInfo)
pWhile = do
  gs <- spanStart
  lexeme (keyword "while")
  cond <- pExpr
  loopBody <- curlyBrackets (P.many pStmtColon)
  info <- spanEnd gs
  pure (WhileX () cond loopBody, info)

pRet :: Parser (StmtPar, SourceInfo)
pRet = do
  gs <- spanStart
  lexeme (keyword "return")
  expr <- P.optional pExpr
  info <- spanEnd gs
  pure (RetX () expr, info)

pBreak :: Parser (StmtPar, SourceInfo)
pBreak = do
  gs <- spanStart
  lexeme (keyword "break")
  expr <- P.optional pExpr
  info <- spanEnd gs
  pure (RetX () expr, info)

pLet :: Parser (StmtPar, SourceInfo)
pLet = do
  gs <- spanStart
  lexeme (keyword "let")
  mut <- maybe Immutable (const Mutable) <$> P.optional (lexeme (keyword "mut"))
  name <- lexeme identifier
  ty <- P.optional $ lexeme (keyword ":") *> pType
  lexeme (keyword "=")
  expr <- pExpr
  info <- spanEnd gs
  pure (LetX (mut, ty) name expr, info)


pAss :: Parser (StmtPar, SourceInfo)
pAss = do
  gs <- spanStart
  name <- P.try $ lexeme identifier <* lexeme (keyword "=")
  expr <- pExpr
  info <- spanEnd gs
  pure (AssX info name expr, info)

pBlock :: Parser (StmtPar, SourceInfo)
pBlock = do
  gs <- spanStart
  stmts <- curlyBrackets (P.many pStmtColon)
  info <- spanEnd gs
  pure (BlockX () stmts, info)

pSExp :: Parser (StmtPar, SourceInfo)
pSExp = do
  gs <- spanStart
  expr <- pExpr
  info <- spanEnd gs
  pure (SExprX () expr, info)

pExpr :: Parser ExprPar
pExpr = putInfo (P.makeExprParser pExprAtom table)
 where
  table =
    [
      [ binaryL "*" (binOp Mul)
      , binaryL "/" (binOp Div)
      , binaryL "%" (binOp Mod)
      ]
    ,
      [ binaryL "+" (binOp Add)
      , binaryL "-" (binOp Sub)
      ]
    ,
      [ binaryL "<=" (binOp Lte)
      , binaryL ">=" (binOp Gte)
      , binaryL "<" (binOp Lt)
      , binaryL ">" (binOp Gt)
      ]
    ,
      [ binaryL "==" (binOp Eq)
      , binaryL "!=" (binOp Neq)
      ]
    ,
      [ binaryL "&&" (binOp And)
      ]
    ,
      [ binaryL "||" (binOp Or)
      ]
    ,
      [ binaryL "=" (binOp Assign)
      , binaryL "+=" (binOp AddAssign)
      , binaryL "*=" (binOp MulAssign)
      , binaryL "/=" (binOp DivAssign)
      , binaryL "%=" (binOp ModAssign)
      ]
    ]

  binaryL :: Text -> (a -> a -> a) -> Operator Parser a
  binaryL name f = InfixL (f <$ lexeme (keyword name))
  binaryR name f = InfixR (f <$ lexeme (keyword name))
  binaryN name f = InfixN (f <$ lexeme (keyword name))
  binOp op l = BinOpX emptyInfo l op

pExprAtom :: Parser ExprPar
pExprAtom =
  P.choice
    [ pLit
    , uncurry (flip EStmtX) <$> pStmt
    , pVar
    , parens pExpr
    ]
 where
  pVar :: Parser ExprPar
  pVar = do
    gs <- spanStart
    name <- identifier
    info <- spanEnd gs
    pure (VarX info name)

pLit :: Parser ExprPar
pLit =
  P.choice
    [ pBool
    , pNumber
    , pChar
    , pString
    ]
 where
  pBool :: Parser ExprPar
  pBool = do
      gs <- spanStart
      res <- BoolLitX () <$> (True <$ keyword "true" <|> False <$ keyword "false")
      info <- spanEnd gs
      pure (LitX info res)

  pNumber :: Parser ExprPar
  pNumber = do
    gs <- spanStart
    res <- (DoubleLitX () <$> P.try P.float) <|> (IntLitX () <$> P.decimal)
    info <- spanEnd gs
    pure (LitX info res)

  pString :: Parser ExprPar
  pString = do
    gs <- spanStart
    res <- StringLitX () <$> stringLiteral
    info <- spanEnd gs
    pure (LitX info res)

  pChar :: Parser ExprPar
  pChar = do
    gs <- spanStart
    res <- CharLitX () <$> charLiteral
    info <- spanEnd gs
    pure (LitX info res)


putInfo :: Parser ExprPar -> Parser ExprPar
putInfo p = do
  gs <- spanStart 
  (p, pos) <- span gs p
  pure $ case p of
    LitX _ l -> LitX pos l
    VarX _ v -> VarX pos v
    BinOpX _ l op r -> BinOpX pos l op r
    EStmtX _ s -> EStmtX pos s
    AppX _ l rs -> AppX pos l rs
    ExprX v -> impossible v
