module Backend.Llvm.Types where

import Data.Data (Data)
import Names (Ident)
import Relude hiding (Type)

newtype Ir = Ir [TopDef]
    deriving (Show)

data TopDef = Fn !Origin !Ident [Operand] !Type [Named Instruction]
    deriving (Show)

data Origin = Toplevel | Lifted
    deriving (Show)

data Operand = Var !Type !Ident | Literal !Lit | Global !Ident
    deriving (Show)

data ArithOp = Add | Sub | Mul | Div | Rem
    deriving (Show)

data CmpOp = Eq | Neq | Gt | Lt | Ge | Le
    deriving (Show)

newtype Label = L Int
    deriving (Show)

data Instruction
    = Call !Type !Operand !Operand [Operand]
    | Arith !ArithOp !Type !Operand !Operand
    | Cmp !CmpOp !Type !Operand !Operand
    | And !Type !Operand !Operand
    | Or !Type !Operand !Operand
    | Alloca !Type
    | Store !Operand
    | Load !Type !Operand
    | Ret !Operand
    | Label !Label
    | Comment !Text
    | Br !Operand !Label !Label
    | Jump !Label
    deriving (Show)

data Type
    = Int
    | Bool
    | Double
    | String
    | Char
    | Unit
    | Pointer Type
    deriving (Show)

data Lit
    = LInt !Integer
    | LDouble !Double
    | LBool !Bool
    | LChar !Char
    | LString !String
    | LUnit
    deriving (Show)

newtype Name = Name Text
    deriving (Show, Generic, Data)

data Named a = Named Name a | Nameless a
    deriving (Show, Functor, Traversable, Foldable, Generic, Data)
