{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Desugar.Pretty where

import Backend.Desugar.Types
import Data.Text (Text)
import Names
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Relude hiding (Text, Type)
import Origin (Origin(Top))

prettyDesugar :: Program -> Text
prettyDesugar = renderStrict . layoutPretty defaultLayoutOptions . pProgram

pType :: Type -> Doc ann
pType = \case
    Unit -> "()"
    String -> "string"
    Char -> "char"
    Int -> "int"
    Double -> "double"
    Bool -> "bool"
    Mut ty -> pType ty <> "?"
    TyFun args ret ->
        sep
            [ "fn" <> parens (hcat (punctuate comma (fmap pType args)))
            , "->"
            , pType ret
            ]

pLit :: Lit -> Doc ann
pLit = \case
    IntLit lit -> show lit
    DoubleLit lit -> show lit
    StringLit lit -> show lit
    CharLit lit -> show lit
    BoolLit True -> "true"
    BoolLit False -> "false"
    UnitLit -> "()"

pBinOp :: BinOp -> Doc ann
pBinOp = \case
    Mul -> "*"
    Div -> "/"
    Add -> "+"
    Sub -> "-"
    Mod -> "%"
    Or -> "||"
    And -> "&&"
    Lt -> "<"
    Gt -> ">"
    Lte -> "<="
    Gte -> ">="
    Eq -> "=="
    Neq -> "!="

pPrefixOp :: PrefixOp -> Doc ann
pPrefixOp = \case
    Not -> "!"
    Neg -> "-"

pProgram :: Program -> Doc ann
pProgram (Program defs) = hcat (punctuate hardline (fmap pDef defs))

pDef :: Def -> Doc ann
pDef (Main exprs) = pDef (Fn Top (Ident "main") [] Unit exprs)
pDef (Fn _ name args typ exprs) =
    concatWith
        (<+>)
        [ "def"
        , pIdent name
        , parens (cat (punctuate comma (fmap pArg args)))
        , "->"
        , pType typ
        , "{"
        , hardline
        , indent 4 $ hcat $ punctuate hardline (fmap pExpr exprs)
        , hardline
        , "}"
        ]

pIdent :: Ident -> Doc ann
pIdent (Ident name) = pretty name

pArg :: Arg -> Doc ann
pArg (Arg name ty) = pIdent name <> ":" <+> pType ty

pExpr :: TyExpr -> Doc ann
pExpr (Typed _ expr) = go expr
  where
    go = \case
        Lit lit -> pLit lit
        Var _ ident -> pIdent ident
        BinOp l op r -> pExpr l <+> pBinOp op <+> pExpr r
        PrefixOp op expr -> pPrefixOp op <+> pExpr expr
        App l rs -> pExpr l <> parens (hcat (punctuate comma $ fmap pExpr rs))
        Let name ty expr -> "let" <+> pIdent name <> ":" <+> pType ty <+> maybe emptyDoc (\e -> "=" <+> pExpr e) expr
        Ass name ty expr -> pIdent name <> ":" <+> pType ty <+> "=" <+> pExpr expr
        Return expr -> "return" <+> pExpr expr
        Break -> "break"
        If cond true mbfalse ->
            let iff =
                    "if"
                        <+> pExpr cond
                        <+> "{"
                        <> hardline
                        <> indent 4 (hcat $ punctuate hardline (fmap pExpr true))
                        <> hardline
                        <> "}"
                els e =
                    "else"
                        <+> "{"
                        <> hardline
                        <> indent 4 (hcat $ punctuate hardline (fmap pExpr e))
                        <> hardline
                        <> "}"
             in case mbfalse of
                    Nothing -> iff
                    Just false -> iff <+> els false
        While cond exprs ->
            "while"
                <+> pExpr cond
                <> "{"
                <> indent 4 (hcat $ punctuate hardline (fmap pExpr exprs))
                <> "}"
