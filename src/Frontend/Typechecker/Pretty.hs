{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend.Typechecker.Pretty where

import Prettyprinter (Doc, Pretty, (<+>))
import Relude

import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty

import Frontend.Typechecker.Types
import Frontend.Types hiding (toList)
import Names (ExplicitName (..))

import Frontend.Types qualified

pThing :: (Pretty a) => a -> Text
pThing = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . Pretty.pretty

instance Pretty ProgramTc where
    pretty (Program NoExtField mods) =
        Pretty.concatWith
            (Pretty.surround (Pretty.hardline <> Pretty.hardline))
            (fmap Pretty.pretty mods)

instance Pretty ModuleTc where
    pretty (Module namespace defs) =
        "===== "
            <> Pretty.pretty namespace
            <> "====="
            <> Pretty.hardline
            <> Pretty.concatWith
                (Pretty.surround (Pretty.hardline <> Pretty.hardline))
                (fmap Pretty.pretty defs)

instance Pretty DefTc where
    pretty (DefFn fn) = Pretty.pretty fn
    pretty (DefAdt adt) = Pretty.pretty adt
    pretty (DefImport import_) = "import" <+> Pretty.pretty import_

instance Pretty ImportTc where
    pretty (ImportExplicit _types namespace names) =
        Pretty.pretty namespace
            <+> Pretty.parens
                ( Pretty.concatWith
                    (Pretty.surround (Pretty.comma <> Pretty.space))
                    (fmap (Pretty.pretty . name) names)
                )

instance Pretty AdtTc where
    pretty (Adt _ name typarams cons) =
        "type"
            <+> Pretty.pretty name.oldname
                <> Pretty.angles (Pretty.pretty typarams)
            <+> Pretty.braces
                ( Pretty.hardline
                    <> Pretty.indent
                        4
                        ( Pretty.concatWith
                            (Pretty.surround Pretty.hardline)
                            (fmap (\x -> Pretty.pretty x <> Pretty.comma) cons)
                        )
                    <> Pretty.hardline
                )

instance Pretty ConstructorTc where
    pretty = \case
        EnumCons _ name -> Pretty.pretty name.oldname
        FunCons _ name types ->
            Pretty.pretty name.oldname
                <> Pretty.parens
                    (Pretty.concatWith (Pretty.surround (Pretty.comma <> Pretty.space)) (fmap Pretty.pretty types))

instance Pretty TyParamList where
    pretty (Params _ []) = ""
    pretty (Params _ xs) =
        Pretty.angles
            $ Pretty.concatWith (Pretty.surround (Pretty.comma <> Pretty.space)) (Pretty.pretty <$> toList xs)

instance Pretty FnTc where
    pretty (Fn _ name tyParams args ty block) =
        "def"
            <+> Pretty.pretty name.oldname
                <> Pretty.pretty tyParams
                <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty args))
            <+> "->"
            <+> Pretty.pretty ty
            <+> Pretty.pretty block

instance Pretty BlockTc where
    pretty (Block _ stmts tail) =
        Pretty.braces
            $ Pretty.hardline
            <> Pretty.indent 4 (Pretty.hcat $ Pretty.punctuate Pretty.hardline (fmap Pretty.pretty stmts))
            <> Pretty.pretty tail
            <> Pretty.hardline

instance Pretty DataConCantHappen where
    pretty _ = error "absurd"

instance Pretty StmtTc where
    pretty (SExpr NoExtField expr) = Pretty.pretty expr <> Pretty.semi

instance Pretty ArgTc where
    pretty (Arg NoExtField name ty) = Pretty.pretty name.oldname <> ":" <+> Pretty.pretty ty

instance Pretty TypeTc where
    pretty = prettyType1

prettyType1 :: TypeTc -> Doc ann
prettyType1 (TyFun _ l r) =
    "fn" <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty l))
        <+> "->"
        <+> Pretty.pretty r
prettyType1 ty = prettyType2 ty

prettyType2 :: TypeTc -> Doc ann
prettyType2 = \case
    TyCon _ name typarams
        | null typarams -> Pretty.pretty name
        | otherwise ->
            Pretty.pretty name
                <> Pretty.angles
                    (Pretty.concatWith (Pretty.surround $ Pretty.comma <> Pretty.space) $ fmap Pretty.pretty typarams)
    PointerType NoExtField ty -> "&" <> Pretty.pretty ty
    TyLit _ tylit -> case tylit of
        Unit -> "()"
        String -> "string"
        Int -> "int"
        Double -> "double"
        Char -> "char"
        Bool -> "bool"
    ty@TyFun {} -> Pretty.parens (Pretty.pretty ty)
    Type AnyX -> "Any"
    TypeVar _ tyvar -> Pretty.pretty tyvar
    Type (Mono mono) -> Pretty.pretty mono

instance Pretty MonoType where
    pretty (MonoType n) = "%" <> show n

instance Pretty ExprTc where
    pretty = prettyExpr1

prettyExpr1 :: ExprTc -> Doc ann
prettyExpr1 (BinOp _ l Or r) = Pretty.pretty l <+> "||" <+> Pretty.pretty r
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: ExprTc -> Doc ann
prettyExpr2 (BinOp _ l And r) = Pretty.pretty l <+> "&&" <+> Pretty.pretty r
prettyExpr2 e = prettyExpr3 e

prettyExpr3 :: ExprTc -> Doc ann
prettyExpr3 (BinOp _ l Eq r) = Pretty.pretty l <+> "==" <+> Pretty.pretty r
prettyExpr3 (BinOp _ l Neq r) = Pretty.pretty l <+> "!=" <+> Pretty.pretty r
prettyExpr3 e = prettyExpr4 e

prettyExpr4 :: ExprTc -> Doc ann
prettyExpr4 (BinOp _ l Lt r) = Pretty.pretty l <+> "<" <+> Pretty.pretty r
prettyExpr4 (BinOp _ l Lte r) = Pretty.pretty l <+> "<=" <+> Pretty.pretty r
prettyExpr4 (BinOp _ l Gt r) = Pretty.pretty l <+> ">" <+> Pretty.pretty r
prettyExpr4 (BinOp _ l Gte r) = Pretty.pretty l <+> ">=" <+> Pretty.pretty r
prettyExpr4 e = prettyExpr5 e

prettyExpr5 :: ExprTc -> Doc ann
prettyExpr5 (BinOp _ l Add r) = Pretty.pretty l <+> "+" <+> Pretty.pretty r
prettyExpr5 (BinOp _ l Sub r) = Pretty.pretty l <+> "-" <+> Pretty.pretty r
prettyExpr5 e = prettyExpr6 e

prettyExpr6 :: ExprTc -> Doc ann
prettyExpr6 (BinOp _ l Mod r) = Pretty.pretty l <+> "%" <+> Pretty.pretty r
prettyExpr6 (BinOp _ l Div r) = Pretty.pretty l <+> "/" <+> Pretty.pretty r
prettyExpr6 (BinOp _ l Mul r) = Pretty.pretty l <+> "*" <+> Pretty.pretty r
prettyExpr6 (Prefix _ Not r) = "!" <+> Pretty.pretty r
prettyExpr6 (Prefix _ Neg r) = "-" <+> Pretty.pretty r
prettyExpr6 e = prettyExpr7 e

prettyExpr7 :: ExprTc -> Doc ann
prettyExpr7 e@BinOp {} = Pretty.parens (Pretty.pretty e)
prettyExpr7 e@Prefix {} = Pretty.parens (Pretty.pretty e)
prettyExpr7 (Lit _ lit) = Pretty.pretty lit
prettyExpr7 (Var (_, ty, _) name) = Pretty.parens $ Pretty.pretty name.oldname <> ":" <+> Pretty.pretty ty
prettyExpr7 (App _ l rs) =
    Pretty.pretty l
        <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty rs))
prettyExpr7 (Let (StmtType {_varType = ty}) name e) =
    "let"
        <+> Pretty.pretty name.oldname
            <> ":"
            <> Pretty.pretty ty
        <+> "="
        <+> Pretty.pretty e
prettyExpr7 (Ass _ name op e) = Pretty.pretty name.oldname <+> Pretty.pretty op <+> Pretty.pretty e
prettyExpr7 (Ret _ Nothing) = "return"
prettyExpr7 (Ret _ (Just e)) = "return" <+> Pretty.pretty e
prettyExpr7 (EBlock _ block) = Pretty.pretty block
prettyExpr7 (Break _ Nothing) = "break"
prettyExpr7 (Break _ (Just e)) = "break" <+> Pretty.pretty e
prettyExpr7 (If _ cond thenB Nothing) = "if" <+> Pretty.pretty cond <+> Pretty.pretty thenB
prettyExpr7 (If a cond thenB (Just elseB)) =
    Pretty.pretty (If a cond thenB Nothing) <+> "else" <+> Pretty.pretty elseB
prettyExpr7 (While _ cond block) = "while" <+> Pretty.pretty cond <+> Pretty.pretty block
prettyExpr7 (Loop _ block) = "loop" <+> Pretty.pretty block
prettyExpr7 (Lam _ args body) =
    "\\" <> Pretty.concatWith (Pretty.surround Pretty.space) (fmap Pretty.pretty args)
        <+> "->"
        <+> Pretty.pretty body
prettyExpr7 (Match _ scrutinee arms) =
    "match"
        <+> Pretty.pretty scrutinee
        <+> Pretty.braces
            ( Pretty.hardline
                <> Pretty.indent
                    4
                    ( Pretty.concatWith
                        (Pretty.surround (Pretty.comma <> Pretty.hardline))
                        (fmap Pretty.pretty arms)
                    )
                <> Pretty.hardline
            )
prettyExpr7 (Expr (TypeApp expr ty_args _)) =
    Pretty.pretty expr
        <> Pretty.angles
            (Pretty.concatWith (Pretty.surround (Pretty.comma <> Pretty.space)) (fmap Pretty.pretty ty_args))

instance Pretty MatchArmTc where
    pretty (MatchArm _ pat expr) = Pretty.pretty pat <+> "=>" <+> Pretty.pretty expr

instance Pretty PatternTc where
    pretty = \case
        PVar _ varName -> Pretty.pretty varName.name
        PEnumCon _ conName -> Pretty.pretty conName.name
        PFunCon _ conName pats ->
            Pretty.pretty conName.oldname
                <> Pretty.parens
                    ( Pretty.concatWith
                        (Pretty.surround (Pretty.comma <> Pretty.space))
                        (fmap Pretty.pretty pats)
                    )

instance Pretty LamArgTc where
    pretty (LamArg ty name) = Pretty.parens (Pretty.pretty name.oldname <+> Pretty.pretty ty)

instance Pretty LitTc where
    pretty lit = case lit of
        IntLit _ l -> Relude.show l
        DoubleLit _ l -> Relude.show l
        StringLit _ l -> Relude.show l
        CharLit _ l -> Relude.show l
        BoolLit _ True -> "true"
        BoolLit _ False -> "false"
        UnitLit _ -> "()"

--
-- tcPrettyStmt :: StmtTc -> Doc ann
-- tcPrettyStmt (SExprX _ e) = prettyExpr1 e

instance Pretty AssignOp where
    pretty = \case
        AddAssign -> "+="
        SubAssign -> "-="
        MulAssign -> "*="
        DivAssign -> "/="
        ModAssign -> "%="
        Assign -> "="
