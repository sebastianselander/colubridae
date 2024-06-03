{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Data.Data (Data)
import Relude hiding (Type, concat, intercalate, replicate)

data Mutability = Mutable | Immutable
  deriving (Show, Eq, Ord)

data ProgramX a = ProgramX (XProgram a) [DefX a]

type family XProgram a

data DefX a = Fn (XDef a) Ident [ArgX a] (TypeX a) [StmtX a]

type family XDef a

data ArgX a = ArgX (XArg a) Ident (TypeX a)

type family XArg a

newtype Ident = Ident Text
  deriving (Show, Eq, Ord, Data)

data TypeX a
  = UnitX (XUnit a)
  | StringX (XString a)
  | IntX (XInt a)
  | DoubleX (XDouble a)
  | CharX (XChar a)
  | BoolX (XBool a)
  | TyVarX (XTyVar a) Ident
  | TyFunX (XTyFun a) (TypeX a) (TypeX a)

type family XUnit a
type family XString a
type family XInt a
type family XDouble a
type family XChar a
type family XBool a
type family XTyVar a
type family XTyFun a

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
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | Assign
  deriving (Show, Eq, Ord, Data)

type family XRet a
type family XBlock a
type family XBreak a
type family XIf a
type family XWhile a
type family XLet a
type family XSExp a
type family XStmt a

data StmtX a
  = RetX (XRet a) (Maybe (ExpX a))
  | BlockX (XBlock a) [StmtX a]
  | BreakX (XBreak a) (Maybe (ExpX a))
  | IfX (XIf a) (ExpX a) [StmtX a] (Maybe [StmtX a])
  | WhileX (XWhile a) (ExpX a) [StmtX a]
  | LetX (XLet a) Ident (ExpX a)
  | SExpX (XSExp a) (ExpX a)
  | StmtX (XStmt a)

type family XExprStmt a
type family XLit a
type family XVar a
type family XBinOp a
type family XApp a

data ExpX a
  = LitX (XLit a) (LitX a)
  | VarX (XVar a) Ident
  | BinOpX (XBinOp a) (ExpX a) BinOp (ExpX a)
  | AppX (XApp a) (ExpX a) [ExpX a]
  | EStmtX (XExprStmt a) (StmtX a)

type family XIntLit a
type family XDoubleLit a
type family XStringLit a
type family XCharLit a
type family XBoolLit a

data LitX a
  = IntLitX (XIntLit a) Integer
  | DoubleLitX (XDoubleLit a) Double
  | StringLitX (XStringLit a) Text
  | CharLitX (XCharLit a) Char
  | BoolLitX (XBoolLit a) Bool
