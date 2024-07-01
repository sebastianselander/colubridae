module Backend.Desugar.Types where

import Data.Data (Data)
import Relude hiding (Type)
import Names (Ident)

newtype Program = Program [Def]
    deriving (Show, Eq, Ord, Data)

data Def = Fn Ident [Arg] Type [TyExpr]
    deriving (Show, Eq, Ord, Data)

data Arg = Arg Ident Type
    deriving (Show, Eq, Ord, Data)

data Type
    = Unit
    | String
    | Char
    | Int
    | Double
    | Bool
    | Mut Type
    | TyFun [Type] Type
    deriving (Show, Eq, Ord, Data)

data TyExpr = Typed Type Expr
    deriving (Show, Eq, Ord, Data)

data Expr
    = Lit Lit
    | Var Binding Ident
    | BinOp TyExpr BinOp TyExpr
    | PrefixOp PrefixOp TyExpr
    | EBlock [TyExpr]
    | App TyExpr [TyExpr]
    | Let Ident Type TyExpr
    | Ass Ident Type TyExpr
    | Ret TyExpr
    | Break TyExpr
    | If TyExpr [TyExpr] (Maybe [TyExpr])
    | While TyExpr [TyExpr]
    deriving (Show, Eq, Ord, Data)

data Binding = Free | Bound | Toplevel | Lambda
    deriving (Show, Eq, Ord, Data)

data BinOp
    = Mul
    | Div
    | Add
    | Sub
    | Mod
    | Or
    | And
    | Lt
    | Gt
    | Lte
    | Gte
    | Eq
    | Neq
    deriving (Show, Eq, Ord, Data)

data PrefixOp = Not | Neg
    deriving (Show, Eq, Ord, Data)

data Lit
    = IntLit !Integer
    | DoubleLit !Double
    | StringLit !Text
    | CharLit !Char
    | BoolLit !Bool
    | UnitLit
    deriving (Show, Eq, Ord, Data)
