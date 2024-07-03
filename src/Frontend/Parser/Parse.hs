{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Frontend.Parser.Parse (parse) where

import Control.Monad.Combinators.Expr (Operator (..))
import Control.Monad.Combinators.Expr qualified as P
import Data.List (foldr1)
import Data.Tuple.Extra (uncurry3)
import Frontend.Parser.Types
import Frontend.Parser.Utils
import Frontend.Parser.Utils qualified as P
import Frontend.Types
import Names (Ident)
import Relude hiding (span)
import Text.Megaparsec (ParseErrorBundle, (<?>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

parse :: String -> Text -> Either (ParseErrorBundle Text CustomParseError) ProgramPar
parse = P.runParser (ProgramX NoExtField <$> (lexeme (return ()) *> P.many pDef <* P.eof))

runP :: (Show a) => Parser a -> Text -> IO ()
runP p input = case P.runParser (p <* P.eof) "" input of
    Left err -> putStrLn $ P.errorBundlePretty err
    Right res -> print res

pDef :: Parser DefPar
pDef = do
    gs <- spanStart
    lexeme (keyword "def")
    name <- lexeme identifier
    args <- parens (commaSep pArg)
    ty <- P.option (TyLitX NoExtField UnitX) (lexeme (keyword "->") *> pType)
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
pType = P.choice [pAtom, pFunTy, parens pType] <?> "type"
  where
    pFunTy :: Parser TypePar
    pFunTy = do
        lexeme $ keyword "fn"
        argTys <- parens (commaSep pType)
        lexeme $ keyword "->"
        TyFunX NoExtField argTys <$> pType

    pAtom :: Parser TypePar
    pAtom = P.choice [pInt, pDouble, pChar, pString, pUnit, pBool]

    pInt :: Parser TypePar
    pInt = TyLitX NoExtField IntX <$ lexeme (keyword "int")

    pDouble :: Parser TypePar
    pDouble = TyLitX NoExtField DoubleX <$ lexeme (keyword "double")

    pChar :: Parser TypePar
    pChar = TyLitX NoExtField CharX <$ lexeme (keyword "char")

    pString :: Parser TypePar
    pString = TyLitX NoExtField StringX <$ lexeme (keyword "string")

    pUnit :: Parser TypePar
    pUnit = TyLitX NoExtField UnitX <$ lexeme (keyword "()")

    pBool :: Parser TypePar
    pBool = TyLitX NoExtField BoolX <$ lexeme (keyword "bool")

-- TODO: Remove needing semicolon after if, loop, while!
pStmtColon :: Parser (Maybe StmtPar)
pStmtColon = do
    lexeme
        $ P.choice
            [ Just <$> pSExp <* semicolon
            , Nothing <$ P.hidden (pEmpty <* semicolon)
            ]

pEmpty :: Parser NoExtField
pEmpty = return NoExtField

pIf :: Parser ExprPar
pIf = P.label "if" $ do
    gs <- spanStart
    lexeme (keyword "if")
    cond <- pExpr
    thenB <- pBlock
    elseB <- P.optional (lexeme (keyword "else") *> pBlock)
    info <- spanEnd gs
    pure (IfX info cond thenB elseB)

pWhile :: Parser ExprPar
pWhile = P.label "while" $ do
    gs <- spanStart
    lexeme (keyword "while")
    cond <- pExpr
    loopBody <- pBlock
    info <- spanEnd gs
    pure (WhileX info cond loopBody)

pLoop :: Parser ExprPar
pLoop = P.label "loop" $ do
    gs <- spanStart
    lexeme (keyword "loop")
    body <- pBlock
    info <- spanEnd gs
    pure $ Loop info body

pRet :: Parser ExprPar
pRet = P.label "return" $ do
    gs <- spanStart
    lexeme (keyword "return")
    expr <- P.optional pExpr
    info <- spanEnd gs
    pure (RetX info expr)

pBreak :: Parser ExprPar
pBreak = P.label "break" $ do
    gs <- spanStart
    lexeme (keyword "break")
    expr <- P.optional pExpr
    info <- spanEnd gs
    pure $ BreakX info expr

pLet :: Parser ExprPar
pLet = P.label "let" $ do
    gs <- spanStart
    lexeme (keyword "let")
    mut <- maybe Immutable (const Mutable) <$> P.optional (lexeme (keyword "mut"))
    name <- lexeme identifier
    ty <- P.optional $ lexeme (keyword ":") *> pType
    lexeme (keyword "=")
    expr <- pExpr
    info <- spanEnd gs
    pure (LetX (info, mut, ty) name expr)

pAss :: Parser (ExprPar -> ExprPar)
pAss = P.label "assignment" $ do
    gs <- spanStart
    (name, op) <- P.try $ do
        name <- lexeme identifier
        assignOp <- lexeme pAssignOp
        pure (name, assignOp)
    info <- spanEnd gs
    pure $ \l -> AssX info name op l

pAssignOp :: Parser AssignOp
pAssignOp =
    P.choice
        [ keyword "=" <* P.notFollowedBy (keyword "=") $> Assign
        , keyword "+=" $> AddAssign
        , keyword "-=" $> SubAssign
        , keyword "*=" $> MulAssign
        , keyword "/=" $> DivAssign
        , keyword "%=" $> ModAssign
        ]

pBlock :: Parser BlockPar
pBlock = uncurry3 BlockX <$> pBlock'

pBlock' :: Parser (SourceInfo, [StmtPar], Maybe ExprPar)
pBlock' = do
    gs <- spanStart
    (stmts, tail) <-
        curlyBrackets
            (P.optionallyEndedBy pStmtColon (pExpr <* P.notFollowedBy semicolon) <|> return ([], Nothing))
    info <- spanEnd gs
    pure (info, catMaybes stmts, tail)

pSExp :: Parser StmtPar
pSExp = SExprX NoExtField <$> pExpr

pApp :: Parser (ExprPar -> ExprPar)
pApp = do
    gs <- spanStart
    args <- parens $ commaSep pExpr
    info <- spanEnd gs
    pure $ \l -> AppX info l args

pLam :: Parser (ExprPar -> ExprPar)
pLam = do
    gs <- spanStart
    keyword "\\"
    args <- P.many lamArg
    keyword "->"
    info <- spanEnd gs
    pure $ Lam info args
  where
    lamArg :: Parser LamArgPar
    lamArg =
        ( do
            ArgX (info, mut) name ty <- parens pArg
            pure (LamArgX (info, mut, Just ty) name)
        )
            <|> ( do
                    gs <- spanStart
                    name <- lexeme identifier
                    info <- spanEnd gs
                    pure $ LamArgX (info, Immutable, Nothing) name
                )

pExpr :: Parser ExprPar
pExpr = putInfo (P.makeExprParser pExprAtom table)
  where
    table =
        [
            [ Postfix $ foldr1 (>>>) <$> P.some pApp
            ]
        ,
            [ Prefix (keyword "!" $> PrefixX emptyInfo Not)
            , Prefix (keyword "-" $> PrefixX emptyInfo Neg)
            ]
        ,
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
            [ Prefix pAss
            , Prefix pLam
            ]
        , []
        ]

    binaryL :: Text -> (a -> a -> a) -> Operator Parser a
    binaryL name f = InfixL (f <$ lexeme (keyword name))
    binaryR name f = InfixR (f <$ lexeme (keyword name))
    binaryN name f = InfixN (f <$ lexeme (keyword name))
    binOp op l = BinOpX emptyInfo l op

pExprAtom :: Parser ExprPar
pExprAtom =
    P.choice
        [ pIf
        , pWhile
        , pRet
        , pLoop
        , pBreak
        , pLit
        , pLet
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
        <?> "literal"
  where
    pUnit :: Parser ExprPar
    pUnit = do
        gs <- spanStart
        res <- UnitLitX NoExtField <$ keyword "()"
        info <- spanEnd gs
        pure (LitX info res)

    pBool :: Parser ExprPar
    pBool = do
        gs <- spanStart
        res <- BoolLitX NoExtField <$> (True <$ keyword "true" <|> False <$ keyword "false")
        info <- spanEnd gs
        pure (LitX info res)

    pNumber :: Parser ExprPar
    pNumber = do
        gs <- spanStart
        res <- (DoubleLitX NoExtField <$> P.try P.float) <|> (IntLitX NoExtField <$> P.decimal)
        info <- spanEnd gs
        pure (LitX info res)

    pString :: Parser ExprPar
    pString = do
        gs <- spanStart
        res <- StringLitX NoExtField <$> stringLiteral
        info <- spanEnd gs
        pure (LitX info res)

    pChar :: Parser ExprPar
    pChar = do
        gs <- spanStart
        res <- CharLitX NoExtField <$> charLiteral
        info <- spanEnd gs
        pure (LitX info res)

putInfo :: Parser ExprPar -> Parser ExprPar
putInfo p = do
    gs <- spanStart
    (p, pos) <- span gs p
    pure $ case p of
        LitX _ l -> LitX pos l
        VarX _ v -> VarX pos v
        PrefixX _ op r -> PrefixX pos op r
        BinOpX _ l op r -> BinOpX pos l op r
        AppX _ l rs -> AppX pos l rs
        LetX (_, mut, ty) name expr -> LetX (pos, mut, ty) name expr
        AssX _ name op expr -> AssX pos name op expr
        RetX _ expr -> RetX pos expr
        EBlockX NoExtField (BlockX _ stmts tail) -> EBlockX NoExtField (BlockX pos stmts tail)
        BreakX _ expr -> BreakX pos expr
        IfX _ cond true false -> IfX pos cond true false
        WhileX _ cond block -> WhileX pos cond block
        Loop _ block -> Loop pos block
        Lam _ args body -> Lam pos args body
