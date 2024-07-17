{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Backend.Llvm.Types where

import Data.Data (Data)
import Names (Ident)
import Origin
import Relude hiding (Type)
import Backend.Types (Type (..))

newtype Ir = Ir [Decl]
    deriving (Show)

-- These are declared in the order we want them defined in the ir file
data Decl
    = LlvmMain [Named Instruction]
    | Define !Origin !Ident [Operand] !Type [Named Instruction]
    | TypeDefinition !Ident !Type
    | GlobalString !Ident !Type !Text
    deriving (Show, Eq, Ord)

data Operand
    = LocalReference !Type !Ident
    | ConstantOperand !Constant
    deriving (Show, Eq, Ord)

data ArithOp = LlvmAdd | LlvmSub | LlvmMul | LlvmDiv | LlvmRem
    deriving (Show, Eq, Ord)

data CmpOp = LlvmEq | LlvmNeq | LlvmGt | LlvmLt | LlvmGe | LlvmLe
    deriving (Show, Eq, Ord)

newtype Label = L Ident
    deriving (Show, Eq, Ord)

data Instruction
    = Call !Type !Operand [Operand]
    | Arith !ArithOp !Type !Operand !Operand
    | Cmp !CmpOp !Type !Operand !Operand
    | And !Type !Operand !Operand
    | Or !Type !Operand !Operand
    | Alloca !Type
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
    deriving (Show, Eq, Ord)

data Constant
    = LInt !Type !Integer
    | LDouble !Type !Double
    | LBool !Type !Bool
    | LChar !Type !Char
    | LUnit
    | LNull !Type
    | LStruct [Constant]
    | Undef !Type
    | GlobalReference !Type !Ident
    deriving (Show, Eq, Ord)

data Named a = Named Ident a | Nameless a
    deriving (Show, Functor, Traversable, Foldable, Generic, Data, Ord, Eq)

class Typed a where
    typeOf :: a -> Type

instance Typed Operand where
    typeOf = \case
        LocalReference ty _ -> ty
        ConstantOperand constant -> typeOf constant

instance Typed Constant where
    typeOf = \case
        LInt ty _ -> ty
        LDouble ty _ -> ty
        LBool ty _ -> ty
        LChar ty _ -> ty
        LUnit -> I 1
        LNull ty -> ty
        LStruct constants -> StructType (fmap typeOf constants)
        Undef ty -> ty
        GlobalReference ty _ -> ty
