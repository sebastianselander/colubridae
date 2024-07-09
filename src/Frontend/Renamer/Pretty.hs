{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend.Renamer.Pretty where

import Frontend.Renamer.Types
import Frontend.Types
import Names (Ident (..))
import Prettyprinter (Doc, Pretty, (<+>))
import Prettyprinter qualified as Pretty
import Relude

instance Pretty ProgramRn where
    pretty (ProgramX NoExtField defs) = Pretty.concatWith (Pretty.surround Pretty.hardline) (fmap Pretty.pretty defs)

instance Pretty DefRn where
    pretty (Fn _ (Ident name) args ty block) =
        "def"
            <+> Pretty.pretty name
                <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty args))
            <+> "->"
            <+> Pretty.pretty ty
            <+> Pretty.pretty block

instance Pretty BlockRn where
    pretty (BlockX _ stmts tail) =
        Pretty.braces
            ( Pretty.hardline
                <> Pretty.indent
                    4
                    ( Pretty.concatWith (Pretty.surround Pretty.hardline) (fmap Pretty.pretty stmts)
                        <> maybe Pretty.emptyDoc (\x -> Pretty.hardline <> Pretty.pretty x) tail
                    )
                <> Pretty.hardline
            )

instance Pretty DataConCantHappen where
    pretty _ = error "absurd"

instance Pretty StmtRn where
    pretty (SExprX NoExtField expr) = Pretty.pretty expr <> Pretty.semi

instance Pretty ArgRn where
    pretty (ArgX (_, mut) name ty) = Pretty.pretty mut <+> Pretty.pretty name <> ":" <+> Pretty.pretty ty

instance Pretty TypeRn where
    pretty = prettyType1

prettyType1 :: TypeRn -> Doc ann
prettyType1 (TyFunX _ l r) =
    "fn" <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty l))
        <+> "->"
        <+> Pretty.pretty r
prettyType1 ty = prettyType2 ty

prettyType2 :: TypeRn -> Doc ann
prettyType2 = \case
    TyLitX _ tylit -> case tylit of
        UnitX -> "()"
        StringX -> "string"
        IntX -> "int"
        DoubleX -> "double"
        CharX -> "char"
        BoolX -> "bool"
    ty@TyFunX {} -> Pretty.parens (Pretty.pretty ty)

instance Pretty ExprRn where
    pretty = prettyExpr1

prettyExpr1 :: ExprRn -> Doc ann
prettyExpr1 (BinOpX _ l Or r) = Pretty.pretty l <+> "||" <+> Pretty.pretty r
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: ExprRn -> Doc ann
prettyExpr2 (BinOpX _ l And r) = Pretty.pretty l <+> "&&" <+> Pretty.pretty r
prettyExpr2 e = prettyExpr3 e

prettyExpr3 :: ExprRn -> Doc ann
prettyExpr3 (BinOpX _ l Eq r) = Pretty.pretty l <+> "==" <+> Pretty.pretty r
prettyExpr3 (BinOpX _ l Neq r) = Pretty.pretty l <+> "!=" <+> Pretty.pretty r
prettyExpr3 e = prettyExpr4 e

prettyExpr4 :: ExprRn -> Doc ann
prettyExpr4 (BinOpX _ l Lt r) = Pretty.pretty l <+> "<" <+> Pretty.pretty r
prettyExpr4 (BinOpX _ l Lte r) = Pretty.pretty l <+> "<=" <+> Pretty.pretty r
prettyExpr4 (BinOpX _ l Gt r) = Pretty.pretty l <+> ">" <+> Pretty.pretty r
prettyExpr4 (BinOpX _ l Gte r) = Pretty.pretty l <+> ">=" <+> Pretty.pretty r
prettyExpr4 e = prettyExpr5 e

prettyExpr5 :: ExprRn -> Doc ann
prettyExpr5 (BinOpX _ l Add r) = Pretty.pretty l <+> "+" <+> Pretty.pretty r
prettyExpr5 (BinOpX _ l Sub r) = Pretty.pretty l <+> "-" <+> Pretty.pretty r
prettyExpr5 e = prettyExpr6 e

prettyExpr6 :: ExprRn -> Doc ann
prettyExpr6 (BinOpX _ l Mod r) = Pretty.pretty l <+> "%" <+> Pretty.pretty r
prettyExpr6 (BinOpX _ l Div r) = Pretty.pretty l <+> "/" <+> Pretty.pretty r
prettyExpr6 (BinOpX _ l Mul r) = Pretty.pretty l <+> "*" <+> Pretty.pretty r
prettyExpr6 (PrefixX _ Not r) = "!" <+> Pretty.pretty r
prettyExpr6 (PrefixX _ Neg r) = "-" <+> Pretty.pretty r
prettyExpr6 e = prettyExpr7 e

prettyExpr7 :: ExprRn -> Doc ann
prettyExpr7 e@BinOpX {} = Pretty.parens (Pretty.pretty e)
prettyExpr7 e@PrefixX {} = Pretty.parens (Pretty.pretty e)
prettyExpr7 (LitX _ lit) = Pretty.pretty lit
prettyExpr7 (VarX _ name) = Pretty.pretty name
prettyExpr7 (AppX _ l rs) =
    Pretty.pretty l
        <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty rs))
prettyExpr7 (LetX (_, Immutable, Nothing) name e) = "let" <+> Pretty.pretty name <+> "=" <+> Pretty.pretty e
prettyExpr7 (LetX (_, Mutable, Nothing) name e) = "let mut" <+> Pretty.pretty name <+> "=" <+> Pretty.pretty e
prettyExpr7 (LetX (_, Immutable, Just ty) name e) = "let" <+> Pretty.pretty name <> ":" <+> Pretty.pretty ty <+> "=" <+> Pretty.pretty e
prettyExpr7 (LetX (_, Mutable, Just ty) name e) = "let mut" <+> Pretty.pretty name <> ":" <+> Pretty.pretty ty <+> "=" <+> Pretty.pretty e
prettyExpr7 (AssX _ (Ident name) op e) = Pretty.pretty name <+> Pretty.pretty op <+> Pretty.pretty e
prettyExpr7 (RetX _ Nothing) = "return"
prettyExpr7 (RetX _ (Just e)) = "return" <+> Pretty.pretty e
prettyExpr7 (EBlockX _ block) = Pretty.pretty block
prettyExpr7 (BreakX _ Nothing) = "break"
prettyExpr7 (BreakX _ (Just e)) = "break" <+> Pretty.pretty e
prettyExpr7 (IfX _ cond thenB Nothing) = "if" <+> Pretty.pretty cond <+> Pretty.pretty thenB
prettyExpr7 (IfX a cond thenB (Just elseB)) = Pretty.pretty (IfX a cond thenB Nothing) <+> "else" <+> Pretty.pretty elseB
prettyExpr7 (WhileX _ cond block) = "while" <+> Pretty.pretty cond <+> Pretty.pretty block
prettyExpr7 (LoopX _ block) = "loop" <+> Pretty.pretty block
prettyExpr7 (LamX _ args body) =
    "\\" <> Pretty.concatWith (Pretty.surround Pretty.space) (fmap Pretty.pretty args)
        <+> "->"
        <+> Pretty.pretty body

instance Pretty LamArgRn where
    pretty (LamArgX (_, mut, Nothing) name) = Pretty.pretty mut <+> Pretty.pretty name
    pretty (LamArgX (_, mut, Just ty) name) = Pretty.parens $ Pretty.pretty mut <+> Pretty.pretty name <> ":" <+> Pretty.pretty ty

instance Pretty LitRn where
    pretty lit = case lit of
        IntLitX _ l -> Relude.show l
        DoubleLitX _ l -> Relude.show l
        StringLitX _ l -> Relude.show l
        CharLitX _ l -> Relude.show l
        BoolLitX _ True -> "true"
        BoolLitX _ False -> "false"
        UnitLitX _ -> "()"

--
-- tcPrettyStmt :: StmtRn -> Doc ann
-- tcPrettyStmt (SExprX _ e) = prettyExpr1 e

instance Pretty AssignOp where
    pretty = \case
        AddAssign -> "+="
        SubAssign -> "-="
        MulAssign -> "*="
        DivAssign -> "/="
        ModAssign -> "%="
        Assign -> "="
