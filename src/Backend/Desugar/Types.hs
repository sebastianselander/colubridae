module Backend.Desugar.Types where

import Data.Data (Data)
import Relude hiding (Type)

newtype Program = Program [Def]
    deriving (Show, Eq, Ord, Data)

data Def = Fn Ident [Arg] Type [Block]
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

data Block = Block [Expr] (Maybe Expr)
    deriving (Show, Eq, Ord, Data)

data Expr
    = Lit Lit
    | Var Binding Ident
    | BinOp Expr BinOp Expr
    | PrefixOp PrefixOp Expr
    | EBlock Block
    | App Expr [Expr]
    | Let Ident Type Expr
    | Ass Ident Expr
    | Ret Expr
    | Break Expr
    | If Expr Block (Maybe Block)
    | While Expr Block
    | Typed Type Expr
    deriving (Show, Eq, Ord, Data)

data Binding = Free | Bound | Toplevel | Lifted
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

newtype Ident = Ident Text
    deriving (Show, Eq, Ord, Data)
