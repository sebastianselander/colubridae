{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Frontend.Parser.Parse (parse) where

import Data.Map qualified as Map
import Data.Tuple.Extra (uncurry3)
import Frontend.Parser.Types
import Frontend.Parser.Utils
import Frontend.Types
import Relude hiding (span)
import Text.Megaparsec (ParseErrorBundle, (<?>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

parse' ::
    BindingPowerTable PrefixOp BinOp Void ->
    String ->
    Text ->
    Either (ParseErrorBundle Text CustomParseError) ProgramPar
parse' table file =
    flip runReader table
        . P.runParserT (ProgramX NoExtField <$> (lexeme (return ()) *> P.many pDef <* P.eof)) file

parse :: String -> Text -> Either (ParseErrorBundle Text CustomParseError) ProgramPar
parse = parse' defaultBindingPowerTable

pAdt :: Parser AdtPar
pAdt = do
    gs <- spanStart
    keyword "type"
    adtName <- lexeme upperIdentifier
    constructors <- curlyBrackets (commaSepEnd pConstructor)
    loc <- spanEnd gs
    pure (AdtX loc adtName constructors)

pConstructor :: Parser ConstructorPar
pConstructor = do
    gs <- spanStart
    constructorName <- upperIdentifier
    argumentTypes <- P.optional (parens (commaSep pType))
    loc <- spanEnd gs
    case argumentTypes of
        Nothing -> pure $ EnumCons loc constructorName
        Just tys -> pure $ FunCons loc constructorName tys

pDef :: Parser DefPar
pDef = DefFn <$> pFunction <|> DefAdt <$> pAdt

pFunction :: Parser FnPar
pFunction = do
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
    name <- lexeme identifier
    lexeme (keyword ":")
    (ty, info) <- span gs pType
    pure (ArgX info name ty)

pType :: Parser TypePar
pType = P.choice [pAtom, pFunTy, pTyCon, parens pType] <?> "type"
  where
    pTyCon :: Parser TypePar
    pTyCon = do
        name <- lexeme upperIdentifier
        pure $ TyConX NoExtField name

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
pStmtColon =
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
    pure $ LoopX info body

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
    name <- lexeme identifier
    ty <- P.optional $ lexeme (keyword ":") *> pType
    lexeme (keyword "=")
    expr <- pExpr
    info <- spanEnd gs
    pure (LetX (info, ty) name expr)

pAss :: Parser ExprPar
pAss = P.label "assignment" $ do
    gs <- spanStart
    (name, op) <- P.try $ do
        name <- lexeme identifier
        assignOp <- lexeme pAssignOp
        pure (name, assignOp)
    info <- spanEnd gs
    AssX info name op <$> pExpr

pMatch :: Parser ExprPar
pMatch = do
    gs <- spanStart
    keyword "match"
    scrutinee <- pExpr
    matchArms <- curlyBrackets $ commaSepEnd pMatchArm
    loc <- spanEnd gs
    pure $ MatchX loc scrutinee matchArms
  where
    pMatchArm :: Parser MatchArmPar
    pMatchArm = do
        gs <- spanStart
        pattern <- pPattern
        keyword "=>"
        body <- pExpr
        loc <- spanEnd gs
        pure $ MatchArmX loc pattern body
      where
        pPattern :: Parser PatternPar
        -- NOTE: Must parse wildcard before normal variable or it will be tried as a variable
        pPattern = P.choice [pPCon, pPVar]
          where
            pPCon :: Parser PatternPar
            pPCon = do
                gs <- spanStart
                name <- upperIdentifier
                arguments <- P.optional (parens (commaSep pPattern))
                loc <- spanEnd gs
                case arguments of
                    Nothing -> pure $ PEnumConX loc name
                    Just args -> pure $ PFunConX loc name args
            pPVar :: Parser PatternPar
            pPVar = do
                gs <- spanStart
                name <- identifier
                loc <- spanEnd gs
                pure $ PVarX loc name

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
            (optionallyEndedBy pStmtColon (pExpr <* P.notFollowedBy semicolon) <|> return ([], Nothing))
    info <- spanEnd gs
    pure (info, catMaybes stmts, tail)

pSExp :: Parser StmtPar
pSExp = SExprX NoExtField <$> pExpr

pLam :: Parser ExprPar
pLam = do
    gs <- spanStart
    keyword "\\"
    args <- lexeme $ P.many lamArg
    keyword "->"
    info <- spanEnd gs
    LamX info args <$> pExpr
  where
    lamArg :: Parser LamArgPar
    lamArg =
        ( do
            ArgX info name ty <- parens pArg
            pure (LamArgX (info, Just ty) name)
        )
            <|> ( do
                    gs <- spanStart
                    name <- lexeme identifier
                    info <- spanEnd gs
                    pure $ LamArgX (info, Nothing) name
                )

pApp :: Parser ExprPar
pApp = do
    gs <- spanStart
    expr <- pExprAtom
    args <- P.many $ (,) <$> spanEnd gs <*> parens (commaSep pExpr)
    let res = foldl' (\l (info, rs) -> AppX info l rs) expr args
    pure res

pExpr :: Parser ExprPar
pExpr = pLam <|> pAss <|> prattExpr pPrefix pInfix pApp

pExprAtom :: Parser ExprPar
pExprAtom =
    P.choice
        [ pMatch
        , pIf
        , pWhile
        , pRet
        , pLoop
        , pBreak
        , pLet
        , EBlockX NoExtField <$> pBlock
        , pLit
        , pVar
        , parens pExpr
        ]
  where
    pVar :: Parser ExprPar
    pVar = do
        gs <- spanStart
        name <- identifier <|> upperIdentifier
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
        res <- DoubleLitX NoExtField <$> P.try P.float <|> IntLitX NoExtField <$> P.decimal
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

pPrefix :: Parser PrefixOp
pPrefix =
    P.choice
        [ keyword "!" $> Not
        , keyword "-" $> Neg
        ]

pInfix :: Parser BinOp
pInfix =
    P.choice
        [ keyword "+" $> Add
        , keyword "-" $> Sub
        , keyword "*" $> Mul
        , keyword "/" $> Div
        , keyword "%" $> Mod
        , keyword "<=" $> Lte
        , keyword ">=" $> Gte
        , keyword "<" $> Lt
        , keyword ">" $> Gt
        , keyword "==" $> Eq
        , keyword "!=" $> Neq
        , keyword "&&" $> And
        , keyword "||" $> Or
        ]

defaultBindingPowerTable :: BindingPowerTable PrefixOp BinOp Void
defaultBindingPowerTable =
    let _infixTable =
            Map.fromList
                [ (Add, (6, 7))
                , (Sub, (6, 7))
                , (Mul, (7, 8))
                , (Div, (7, 8))
                , (Mod, (7, 8))
                , (Lte, (4, 4))
                , (Gte, (4, 4))
                , (Lt, (4, 4))
                , (Gt, (4, 4))
                , (Eq, (4, 4))
                , (Neq, (4, 4))
                , (And, (4, 3))
                , (Or, (3, 2))
                ]
        _prefixTable = Map.fromList [(Not, 9), (Neg, 9)]
        _postfixTable = Map.empty
     in BindingPowerTable
            { _prefixTable
            , _infixTable
            , _postfixTable
            }

prattExpr :: Parser PrefixOp -> Parser BinOp -> Parser ExprPar -> Parser ExprPar
prattExpr prefixParser infixParser atomParser = exprbp 0
  where
    exprbp :: Int -> Parser ExprPar
    exprbp minBp = do
        gs <- spanStart
        l <- P.eitherP atomParser prefixParser
        case l of
            Left l -> go gs minBp l
            Right r -> do
                bp <- prefixBindingPower r
                rhs <- exprbp bp
                info <- spanEnd gs
                gs <- spanStart
                go gs minBp $ PrefixX info r rhs
      where
        go :: GhostSpan Before -> Int -> ExprPar -> Parser ExprPar
        go gs minBp l = P.withRecovery (\_ -> pure l) $ do
            operator <- P.lookAhead infixParser
            (lbp, rbp) <- infixBindingPower operator
            if lbp < minBp
                then pure l
                else do
                    _ <- pInfix
                    r <- exprbp rbp
                    info <- spanEnd gs
                    go gs minBp (BinOpX info l operator r)
