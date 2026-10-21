{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Backend.Llvm.Lower where

import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Relude hiding (Type)

import Backend.Llvm.Types
import Backend.Types (Type (..))
import Impossible (__IMPOSSIBLE__)
import Names (ExplicitName (..))

target :: Doc ann
target = "target triple = \"x86_64-pc-linux-gnu\""

libc :: Doc ann
libc =
    hcat
        $ punctuate
            hardline
            [ "declare void @exit(i64)"
            , "declare i32 @printf(ptr, ...)"
            , "declare ptr @malloc(i64)"
            ]

llvmOut :: (Pretty a) => a -> Text
llvmOut = renderStrict . layoutPretty (LayoutOptions {layoutPageWidth = Unbounded}) . pretty

indentLevel :: Int
indentLevel = 4

indentedBlock :: BasicBlock -> Doc ann
indentedBlock BasicBlock {label, instructions, terminator} =
    pretty label
        <> ":"
        <> hardline
        <> indent
            indentLevel
            (hcat $ punctuate hardline (fmap pretty instructions <> [pretty terminator]))

indentedBlocks :: [BasicBlock] -> Doc ann
indentedBlocks blocks = concatWith (surround hardline) $ fmap indentedBlock blocks

instance Pretty Module where
    pretty (Module decls) =
        target
            <> hardline
            <> libc
            <> hardline
            <> hcat (punctuate (hardline <> hardline) $ fmap pretty decls)

instance Pretty Definition where
    pretty (TypeDefinition name ty) =
        ";the type inside the array is just used to allocate the correct amount of bytes"
            <> hardline
            <> "%"
            <> prettyName name
            <+> "= type"
            <+> pretty ty
    pretty (GlobalString name ty string) = "@" <> prettyName name <+> "= constant" <+> pretty ty <+> "c" <+> dquotes (pretty string)
    pretty (Function _ name args ty blocks) =
        "define"
            <+> "dso_local"
            <+> pretty ty
            <+> "@"
            <> prettyName name
            <> tupled (fmap typed args)
            <+> lbrace
            <> hardline
            <> indentedBlocks blocks
            <> hardline
            <> rbrace
    pretty (Declare ty name args ellipsis) =
        "declare"
            <+> pretty ty
            <+> "@"
            <> prettyName name
            <> parens
                ( concatWith
                    (surround (comma <> space))
                    (fmap pretty args)
                    <> if ellipsis == Ellipsis then ", ..." else ""
                )

instance Pretty Type where
    pretty = \case
        I n -> "i" <> show n
        Float -> "double"
        ArrayType size ty -> brackets $ show size <+> "x" <+> pretty ty
        Void -> "void"
        OpaquePointer -> "ptr"
        PointerType OpaquePointer -> "ptr"
        PointerType Void -> "ptr"
        PointerType ty -> pretty ty <> "*"
        TyFun tys ty -> pretty ty <> tupled (fmap pretty tys) <> "*"
        StructType tys -> braces $ space <> concatWith (surround (comma <> space)) (fmap pretty tys) <> space
        Mut ty -> pretty ty
        TyCon name -> "%" <> prettyName name

instance Pretty Constant where
    pretty = \case
        LInt _ int -> show int
        LDouble _ double -> show double
        LBool _ b -> bool "false" "true" b
        LChar _ c -> show (ord c)
        LUnit -> "0"
        LNull _ -> "null"
        LStruct constants ->
            braces
                $ concatWith (surround (comma <> space))
                $ fmap (\x -> pretty (typeOf x) <+> pretty x) constants
        Undef _ -> "undef"
        Poison _ -> "poison"
        GlobalReference _ name -> "@" <> prettyName name

instance Pretty Instruction where
    pretty = \case
        Blankline -> hardline
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
                    OpaquePointer -> OpaquePointer
                    _ -> OpaquePointer -- __IMPOSSIBLE__
            "load" <+> pretty ty <> "," <+> typed operand
        Comment cmnt -> ";" <+> pretty cmnt
        GetElementPtr op ops -> case typeOf op of
            PointerType ty -> "getelementptr" <+> pretty ty <> "," <+> typed op <> "," <+> commasep (fmap typed ops)
            ty -> __IMPOSSIBLE__
        ExtractValue operand indices -> "extractvalue" <+> typed operand <> "," <+> commasep (fmap pretty indices)
        Malloc operand -> "call ptr @malloc" <> parens (typed operand)
        Phi [] -> __IMPOSSIBLE__
        Phi xs@(i : _) ->
            "phi"
                <+> pretty (typeOf (fst i))
                <+> concatWith
                    (surround (comma <> space))
                    ( fmap
                        (\(operand, label) -> brackets (space <> pretty operand <> comma <+> "%" <> pretty label <> space))
                        xs
                    )

instance Pretty Terminator where
    pretty = \case
        Br operand lbl1 lbl2 -> "br" <+> typed operand <> "," <+> "label %" <> pretty lbl1 <> "," <+> "label %" <> pretty lbl2
        Ret operand -> "ret" <+> maybe "i1 0" typed operand
        Jump lbl -> "br label %" <> pretty lbl
        Unreachable -> "unreachable"
        Switch op lbl pairs ->
            "switch"
                <+> typed op
                <> comma
                <+> "label %"
                <> pretty lbl
                <+> brackets
                    ( concatWith
                        (surround space)
                        (fmap ((\(l, r) -> l <> comma <+> "label %" <> r) . bimap typed pretty) pairs)
                    )

prettyName :: ExplicitName -> Doc ann
prettyName name
    | name.oldname == "main" = "main"
    | otherwise = dquotes $ pretty name

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
        LocalReference _ name -> "%" <> prettyName name
        ConstantOperand constant -> pretty constant

instance (Pretty a) => Pretty (Named a) where
    pretty (Named name a) = "%" <> prettyName name <+> "=" <+> pretty a
    pretty (Nameless a) = pretty a
