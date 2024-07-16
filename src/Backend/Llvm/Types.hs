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
    deriving (Show)

data Operand
    = LocalReference !Type !Ident
    | ConstantOperand !Constant
    deriving (Show)

data ArithOp = LlvmAdd | LlvmSub | LlvmMul | LlvmDiv | LlvmRem
    deriving (Show)

data CmpOp = LlvmEq | LlvmNeq | LlvmGt | LlvmLt | LlvmGe | LlvmLe
    deriving (Show)

newtype Label = L Ident
    deriving (Show)

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
    deriving (Show)

data Constant
    = LInt !Type !Integer
    | LDouble !Type !Double
    | LBool !Type !Bool
    | LChar !Type !Char
    | LUnit
    | LNull !Type
    | GlobalReference !Type !Ident
    deriving (Show)

data Named a = Named Ident a | Nameless a
    deriving (Show, Functor, Traversable, Foldable, Generic, Data)

class Typed a where
    typeOf :: a -> Type
    setType :: Type -> a -> a

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
        LUnit -> I 1
        LNull ty -> ty
        GlobalReference ty _ -> ty
