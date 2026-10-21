{-# LANGUAGE LambdaCase #-}

module Backend.Llvm.Types where

import Data.Data (Data)
import Relude hiding (Type)

import Backend.Types (Type (..))
import Names (ExplicitName)
import Origin

newtype Module = Module {_decls :: [Definition]}
    deriving (Show)

updateDecls :: ([Definition] -> [Definition]) -> Module -> Module
updateDecls f (Module decls) = Module (f decls)

data Ellipsis = Ellipsis | NoEllipsis
    deriving (Data, Eq, Ord, Show)

type IsBuiltin = Bool

-- These are declared in the order we want them defined in the ir file
data Definition
    = Function !Origin !ExplicitName [Operand] !Type [Named Instruction]
    | TypeDefinition !ExplicitName !Type
    | GlobalString !ExplicitName !Type !Text
    | Declare
        !Type -- return type
        ExplicitName -- name
        [Type] -- argument types
        !Ellipsis -- varargs?
    deriving (Eq, Ord, Show)

data Operand
    = LocalReference !Type !ExplicitName
    | ConstantOperand !Constant
    deriving (Eq, Ord, Show)

data ArithOp = LlvmAdd | LlvmSub | LlvmMul | LlvmDiv | LlvmRem
    deriving (Eq, Ord, Show)

data CmpOp = LlvmEq | LlvmNeq | LlvmGt | LlvmLt | LlvmGe | LlvmLe
    deriving (Eq, Ord, Show)

newtype Label = L Text
    deriving (Eq, Ord, Show)

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
    | Phi [(Operand, Label)]
    | Switch !Operand Label [(Constant, Label)]
    | Blankline
    | Unreachable
    deriving (Eq, Ord, Show)

data Constant
    = LInt !Type !Integer
    | LDouble !Type !Double
    | LBool !Type !Bool
    | LChar !Type !Char
    | LUnit
    | LNull !Type
    | LStruct [Constant]
    | Undef !Type
    | Poison !Type
    | GlobalReference !Type !ExplicitName
    deriving (Eq, Ord, Show)

data Named a = Named ExplicitName a | Nameless a
    deriving (Data, Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

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
        Poison ty -> ty
        GlobalReference ty _ -> ty
