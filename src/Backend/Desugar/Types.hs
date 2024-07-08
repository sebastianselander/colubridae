module Backend.Desugar.Types where

import Data.Data (Data)
import Relude hiding (Type)
import Names (Ident)
import Origin (Origin)

newtype Program = Program [Def]
    deriving (Show, Eq, Ord, Data)

data Def = Fn !Origin Ident [Arg] Type [TyExpr]
         | Main [TyExpr]
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
    | Tuple [Type]
    | VoidPtr
    deriving (Show, Eq, Ord, Data)

data TyExpr = Typed Type Expr
    deriving (Show, Eq, Ord, Data)

data Expr
    = Lit Lit
    | Var Binding Ident
    | BinOp TyExpr BinOp TyExpr
    | PrefixOp PrefixOp TyExpr
    | App TyExpr [TyExpr]
    | Let Ident Type (Maybe TyExpr)
    | Ass Ident Type TyExpr
    | Return TyExpr
    | Break
    | If TyExpr [TyExpr] (Maybe [TyExpr])
    | While TyExpr [TyExpr]
    | Closure TyExpr [TyExpr]
    | PtrIndexing TyExpr Integer
    | StructIndexing TyExpr Integer
    deriving (Show, Eq, Ord, Data)

data Binding = Free | Bound | Toplevel | Argument | GlblConst | Lambda
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
    | CharLit !Char
    | BoolLit !Bool
    | UnitLit
    | NullLit
    deriving (Show, Eq, Ord, Data)
