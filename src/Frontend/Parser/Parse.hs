{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Frontend.Parser.Parse where

import Control.Monad.Combinators.Expr (Operator (..))
import Control.Monad.Combinators.Expr qualified as P
import Data.Text (pack)
import Data.Tuple.Extra (uncurry3)
import Frontend.Parser.Types
import Frontend.Parser.Utils
import Frontend.Parser.Utils qualified as P
import Relude hiding (span)
import Text.Megaparsec (customFailure)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Types
import Utils (impossible)

pProgram :: String -> Text -> Either Text ProgramPar
pProgram file source =
    case flip runReader False $ P.runParserT (ProgramX () <$> P.many pDef <* P.eof) file source of
        Left err -> Left $ pack $ P.errorBundlePretty err
        Right prog -> Right prog

runP :: (Show a) => Parser a -> Text -> IO ()
runP p input = case flip runReader False $ P.runParserT (p <* P.eof) "" input of
    Left err -> putStrLn $ P.errorBundlePretty err
    Right res -> print res

pDef :: Parser DefPar
pDef = do
    gs <- spanStart
    lexeme (keyword "def")
    name <- lexeme identifier
    args <- parens (commaSep pArg)
    ty <- P.option (TyLitX () UnitX) (lexeme (keyword "->") *> pType)
    expressions <- pBlock
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
    pInt = TyLitX () IntX <$ lexeme (keyword "int")

    pDouble :: Parser TypePar
    pDouble = TyLitX () DoubleX <$ lexeme (keyword "double")

    pChar :: Parser TypePar
    pChar = TyLitX () CharX <$ lexeme (keyword "char")

    pString :: Parser TypePar
    pString = TyLitX () StringX <$ lexeme (keyword "string")

    pUnit :: Parser TypePar
    pUnit = TyLitX () UnitX <$ lexeme (keyword "()")

    pTyVar :: Parser TypePar
    pTyVar = TyVarX () <$> identifier

pStmtColon :: Parser (Maybe StmtPar)
pStmtColon = do
    lexeme
        $ P.choice
            [ Just <$> pIf
            , Just <$> pWhile
            , Just <$> pRet <* semicolon
            , Just <$> pBreak <* semicolon
            , Just <$> pLet <* semicolon
            , Just . SBlockX () <$> pBlock
            , Just <$> pAss <* semicolon
            , Just <$> pSExp <* semicolon
            , Nothing <$ (pEmpty <* semicolon)
            ]

pStmt :: Parser StmtPar
pStmt =
    P.choice
        [ pIf
        , pWhile
        , pLet
        , SBlockX () <$> pBlock
        , pAss
        ]

pEmpty :: Parser ()
pEmpty = return ()

pIf :: Parser StmtPar
pIf = do
    gs <- spanStart
    lexeme (keyword "if")
    cond <- pExpr
    thenB <- pBlock
    elseB <- P.optional (lexeme (keyword "else") *> pBlock)
    info <- spanEnd gs
    pure (IfX info cond thenB elseB)

pWhile :: Parser StmtPar
pWhile = do
    gs <- spanStart
    lexeme (keyword "while")
    cond <- pExpr
    loopBody <- local (const True) pBlock
    info <- spanEnd gs
    pure (WhileX info cond loopBody)

pRet :: Parser StmtPar
pRet = do
    gs <- spanStart
    lexeme (keyword "return")
    expr <- P.optional pExpr
    info <- spanEnd gs
    pure (RetX info expr)

pBreak :: Parser StmtPar
pBreak = do
    gs <- spanStart
    lexeme (keyword "break")
    unlessM ask $ customFailure BreakNotInLoop
    expr <- P.optional pExpr
    info <- spanEnd gs
    pure $ BreakX info expr

pLet :: Parser StmtPar
pLet = do
    gs <- spanStart
    lexeme (keyword "let")
    mut <- maybe Immutable (const Mutable) <$> P.optional (lexeme (keyword "mut"))
    name <- lexeme identifier
    ty <- P.optional $ lexeme (keyword ":") *> pType
    lexeme (keyword "=")
    expr <- pExpr
    info <- spanEnd gs
    pure (LetX (info, mut, ty) name expr)

pAss :: Parser StmtPar
pAss = do
    gs <- spanStart
    name <- P.try $ lexeme identifier <* lexeme (keyword "=")
    expr <- pExpr
    info <- spanEnd gs
    pure (AssX info name expr)

pBlock :: Parser BlockPar
pBlock = uncurry3 BlockX <$> pBlock'

pBlock' :: Parser (SourceInfo, [StmtPar], Maybe ExprPar)
pBlock' = do
    gs <- spanStart
    -- TODO: Only allow non-statement expressions
    (stmts, tail) <- curlyBrackets (P.optionallyEndedBy pStmtColon (pExpr <* P.notFollowedBy semicolon))
    info <- spanEnd gs
    pure (info, catMaybes stmts, tail)

pSExp :: Parser StmtPar
pSExp = SExprX () <$> pExpr

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
        , EStmtX () <$> pStmt
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
        , pUnit
        ]
  where
    pUnit :: Parser ExprPar
    pUnit = do
        gs <- spanStart
        res <- UnitLitX <$> keyword "()"
        info <- spanEnd gs
        pure (LitX info res)

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
        EStmtX _ s -> EStmtX () s
        AppX _ l rs -> AppX pos l rs
        ExprX v -> impossible v
