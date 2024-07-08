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
llvmOut = (prelude <>) . renderStrict . layoutPretty (LayoutOptions {layoutPageWidth=Unbounded}) . pretty

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
            <> tupled (fmap typed args)
            <+> lbrace
            <> hardline
            <> indentedBlock instr
            <> hardline
            <> rbrace

instance Pretty LlvmType where
    pretty = \case
        I64 -> "i64"
        I32 -> "i32"
        I1 -> "i1"
        Float -> "double"
        I8 -> "i8"
        LlvmVoid -> "void"
        PointerType (PointerType _) -> "ptr"
        PointerType LlvmVoid -> "ptr"
        PointerType ty -> pretty ty <> "*"
        FunPtr ty tys ->  pretty ty <> tupled (fmap pretty tys) <> "*"
        ArrayType tys -> braces $ concatWith (surround (comma <> space)) $ fmap pretty tys

instance Pretty Constant where
    pretty = \case
        LInt _ int -> show int
        LDouble _ double -> show double
        LBool _ bool -> show (fromEnum bool)
        LChar _ _ -> error "TODO"
        LUnit -> "1"
        LNull _ -> "null"
        GlobalReference _ name -> "@" <> pretty name

instance Pretty Instruction where
    pretty = \case
        Call ty fun args -> "call" <+> pretty ty <+> pretty fun <> tupled (fmap typed args)
        Arith operator ty l r -> pretty operator <+> pretty ty <+> pretty l <> "," <+> pretty r
        Cmp operator _ l r -> "icmp" <+> pretty operator <+> pretty (typeOf l) <+> pretty l <> "," <+> pretty r
        And ty l r -> "and" <+> pretty ty <+> pretty l <> "," <+> pretty r
        Or ty l r -> "or" <+> pretty ty <+> pretty l <> "," <+> pretty r
        Alloca ty -> "alloca" <+> pretty ty
        Store l r -> "store" <+> typed l <> "," <+> typed r
        Load operand -> do
                let ty = case typeOf operand of
                            PointerType ty -> ty
                            _ -> error "Non-pointer"
                "load" <+> pretty ty <> "," <+> typed operand
        Ret operand -> "ret" <+> typed operand
        Label lbl -> pretty lbl <> ":"
        Comment cmnt -> ";" <+> pretty cmnt
        Br operand lbl1 lbl2 -> "br" <+> typed operand <> "," <+> "label %" <> pretty lbl1 <> "," <+> "label %" <> pretty lbl2
        Jump lbl -> "br label %" <> pretty lbl
        GetElementPtr op ops -> case typeOf op of
            PointerType ty -> "getelementptr" <+> pretty ty <> "," <+> typed op <> "," <+> commasep (fmap typed ops)
            ty -> error $ "Non-pointer: '" <> show (pretty ty) <> "' can not be used in GEP"
        ExtractValue operand indices -> "extractvalue" <+> typed operand <> "," <+> commasep (fmap pretty indices)
        Malloc _ -> undefined
        Unreachable -> "unreachable"

commasep :: [Doc ann] -> Doc ann
commasep = concatWith (surround (comma <> space))

typed :: (Pretty a, Typed a) => a -> Doc ann
typed a = pretty (typeOf a) <+> pretty a

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
        LocalReference _ name -> "%" <> pretty name
        ConstantOperand constant -> pretty constant

instance (Pretty a) => Pretty (Named a) where
    pretty (Named name a) = "%" <> pretty name <+> "=" <+> pretty a
    pretty (Nameless a) = pretty a
