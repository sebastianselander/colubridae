{-# LANGUAGE LambdaCase #-}
module Backend.Llvm.Types where

import Data.Data (Data)
import Names (Ident)
import Origin
import Relude hiding (Type)

newtype Ir = Ir [Decl]
    deriving (Show)

data Decl = Define !Origin !Ident [Operand] !LlvmType [Named Instruction]
          | LlvmMain [Named Instruction]
    deriving (Show)

data Operand
    = Variable !LlvmType !Ident
    | Literal !LlvmType !Literal
    | Global !LlvmType !Ident
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
    | Store !Operand !Operand
    | Load !Operand
    | Ret !Operand
    | Label !Label
    | Comment !Text
    | Br !Operand !Label !Label
    | Jump !Label
    | Unreachable
    deriving (Show)

data LlvmType
    = I64
    | I1
    | Float
    | I8
    | Ptr LlvmType
    | FunPtr LlvmType [LlvmType]
    deriving (Show)

data Literal
    = LInt !Integer
    | LDouble !Double
    | LBool !Bool
    | LChar !Char
    | LUnit
    deriving (Show)

data Named a = Named Ident a | Nameless a
    deriving (Show, Functor, Traversable, Foldable, Generic, Data)

class Typed a where
  typeOf :: a -> LlvmType

instance Typed Operand where
  typeOf = \case
    Variable ty _ -> ty
    Global ty _ -> ty
    Literal ty _ -> ty
