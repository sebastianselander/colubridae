module Backend.Intermediate.Types where

import Relude hiding (Type)
import Types (Ident)

newtype Ir = Ir [TopDef]
    deriving (Show)

data TopDef
    = Fn Origin Ident [Arg] Type [Stmt]
    deriving (Show)

data Origin = Toplevel | Lifted
    deriving (Show)

data Arg = Arg Ident Type
    deriving (Show)

data Var = Var Ident | Const Lit
    deriving (Show)

data ArithOp = Add | Sub | Mul | Div | Rem
    deriving (Show)

data CmpOp = Eq | Neq | Gt | Lt | Ge | Le
    deriving (Show)

newtype Label = L Int
    deriving (Show)

data Stmt
    = Call Var Type Var [Arg]
    | Arith Var ArithOp Type Arg Arg
    | Cmp Var CmpOp Type Arg Arg
    | And Var Type Arg Arg
    | Or Var Type Arg Arg
    | Alloca Var Type
    | Store Arg Var
    | Load Var Type Var
    | Ret Arg
    | Label Label
    | Comment Text
    | Br Var Label Label
    | Jump Label
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
    = LInt Integer
    | LDouble Double
    | LBool Bool
    | LChar Char
    | LString String
    | LUnit
    deriving (Show)
