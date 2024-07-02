{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Backend.Llvm.ToLlvm where

import Prettyprinter

import Backend.Llvm.Prelude (prelude)
import Backend.Llvm.Types
import Prettyprinter.Render.Text (renderStrict)
import Relude

llvmOut :: (Pretty a) => a -> Text
llvmOut = (prelude <>) . renderStrict . layoutPretty defaultLayoutOptions . pretty

indentLevel :: Int
indentLevel = 4

indentedBlock :: [Named Instruction] -> Doc ann
indentedBlock instr = indent indentLevel (hcat $ punctuate hardline (fmap pretty instr))

instance Pretty Ir where
    pretty (Ir decls) = hcat $ punctuate (hardline <> hardline) $ fmap pretty decls

instance Pretty Decl where
    pretty (LlvmMain instr) =
        "define void @main()"
            <+> lbrace
            <> hardline
            <> indentedBlock instr
            <> hardline
            <> indent 4 "ret void"
            <> hardline
            <> rbrace
    pretty (Define _ name args ty instr) =
        "define"
            <+> pretty ty
            <+> "@"
            <> pretty name
            <> tupled (fmap pretty args)
            <+> lbrace
            <> hardline
            <> indentedBlock instr
            <> hardline
            <> rbrace

instance Pretty LlvmType where
    pretty = \case
        I64 -> "i64"
        I1 -> "i1"
        Float -> "double"
        I8 -> "i8"
        Ptr ty -> pretty ty <> "*"
        FunPtr ty tys -> pretty ty <> tupled (fmap pretty tys) <> "*"

instance Pretty Literal where
    pretty = \case
        LInt int -> show int
        LDouble double -> show double
        LBool bool -> show $ fromEnum bool
        LChar _ -> error "TODO"
        LUnit -> "1"

instance Pretty Instruction where
    pretty = \case
        Call ty fun args -> "call" <+> pretty ty <+> tyLess fun <> tupled (fmap pretty args)
        Arith op ty l r -> pretty op <+> pretty ty <+> tyLess l <> "," <+> tyLess r
        Cmp op ty l r -> "icmp" <+> pretty op <+> pretty ty <+> pretty l <> "," <+> pretty r
        And ty l r -> "and" <+> pretty ty <+> pretty l <> "," <+> pretty r
        Or ty l r -> "or" <+> pretty ty <+> pretty l <> "," <+> pretty r
        Alloca ty -> "alloca" <+> pretty ty
        Store l r -> "store" <+> pretty l <> "," <+> pretty r
        Load op -> do
            let ty = case typeOf op of
                    Ptr ty -> ty
                    _ -> error "Non-pointer"
            "load" <+> pretty ty <> "," <+> pretty op
        Ret op -> "ret" <+> pretty op
        Label lbl -> indent (negate indentLevel) $ pretty lbl <> ":"
        Comment cmnt -> ";" <+> pretty cmnt
        Br op lbl1 lbl2 -> "br" <+> pretty op <> "," <+> "label %" <> pretty lbl1 <> "," <+> "label %" <> pretty lbl2
        Jump lbl -> "br label %" <> pretty lbl
        Unreachable -> "unreachable"

tyLess :: Operand -> Doc ann
tyLess = \case
    Variable _ name -> "%" <> pretty name
    Literal _ lit -> pretty lit
    Global _ name -> "@" <> pretty name

instance Pretty Label where
    pretty (L name) = pretty name

instance Pretty CmpOp where
    pretty = \case
        LlvmEq -> "eq"
        LlvmNeq -> "neq"
        LlvmGt -> "sgt"
        LlvmLt -> "slt"
        LlvmGe -> "sge"
        LlvmLe -> "sle"

instance Pretty ArithOp where
    pretty = \case
        LlvmAdd -> "add"
        LlvmSub -> "sub"
        LlvmMul -> "mul"
        LlvmRem -> "urem"
        LlvmDiv -> "udiv"

instance Pretty Operand where
    pretty = \case
        Variable ty name -> pretty ty <+> "%" <> pretty name
        Literal ty lit -> pretty ty <+> pretty lit
        Global ty name -> pretty ty <+> "@" <> pretty name

instance (Pretty a) => Pretty (Named a) where
    pretty (Named name a) = "%" <> pretty name <+> "=" <+> pretty a
    pretty (Nameless a) = pretty a
