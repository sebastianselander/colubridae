{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Backend.Llvm.Types where

import Data.Data (Data)
import Names (Ident)
import Origin
import Relude hiding (Type)

newtype Ir = Ir [Decl]
    deriving (Show)

data Decl
    = Define !Origin !Ident [Operand] !LlvmType [Named Instruction]
    | LlvmMain [Named Instruction]
    | GlobalString !Ident !LlvmType !Text
    deriving (Show)

data Operand
    = LocalReference !LlvmType !Ident
    | ConstantOperand !Constant
    deriving (Show)

data ArithOp = LlvmAdd | LlvmSub | LlvmMul | LlvmDiv | LlvmRem
    deriving (Show)

data CmpOp = LlvmEq | LlvmNeq | LlvmGt | LlvmLt | LlvmGe | LlvmLe
    deriving (Show)

newtype Label = L Ident
    deriving (Show)

data Instruction
    = Call !LlvmType !Operand [Operand]
    | Arith !ArithOp !LlvmType !Operand !Operand
    | Cmp !CmpOp !LlvmType !Operand !Operand
    | And !LlvmType !Operand !Operand
    | Or !LlvmType !Operand !Operand
    | Alloca !LlvmType
    | Malloc !Operand
    | Store !Operand !Operand
    | Load !Operand
    | Ret !Operand
    | Label !Label
    | Comment !Text
    | Br !Operand !Label !Label
    | Jump !Label
    | GetElementPtr !Operand [Operand]
    | ExtractValue !Operand [Word32]
    | Blankline
    | Unreachable
    deriving (Show)

data LlvmType
    = I64
    | I32
    | I1
    | Float
    | I8
    | PointerType LlvmType
    | BlindPointerType
    | LlvmVoid
    | StructType [LlvmType]
    | ArrayType Integer LlvmType
    | FunPtr LlvmType [LlvmType]
    deriving (Show)

data Constant
    = LInt !LlvmType !Integer
    | LDouble !LlvmType !Double
    | LBool !LlvmType !Bool
    | LChar !LlvmType !Char
    | LUnit
    | LNull !LlvmType
    | GlobalReference !LlvmType !Ident
    deriving (Show)

data Named a = Named Ident a | Nameless a
    deriving (Show, Functor, Traversable, Foldable, Generic, Data)

class Typed a where
    typeOf :: a -> LlvmType
    setType :: LlvmType -> a -> a

instance Typed Operand where
    typeOf = \case
        LocalReference ty _ -> ty
        ConstantOperand constant -> typeOf constant
    setType ty = \case
        LocalReference _ v -> LocalReference ty v
        ConstantOperand constant -> ConstantOperand $ setType ty constant

instance Typed Constant where
    setType ty = \case
        LInt _ v -> LInt ty v
        LDouble _ v -> LDouble ty v
        LBool _ v -> LBool ty v
        LChar _ v -> LChar ty v
        LUnit -> error "Setting type of Unit is not possible"
        LNull _ -> LNull ty
        GlobalReference _ v -> GlobalReference ty v
    typeOf = \case
        LInt ty _ -> ty
        LDouble ty _ -> ty
        LBool ty _ -> ty
        LChar ty _ -> ty
        LUnit -> I1
        LNull ty -> ty
        GlobalReference ty _ -> ty
