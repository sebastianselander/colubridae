{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Data (Data)
import Relude hiding (Type)

data UD

newtype Program a = Program [DefX a]

data DefX a = Fn (XDef a) Ident [ArgX a] (TypeX a) [ExpX a]

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

data ArithOp
  = Mul
  | Div
  | Add
  | Sub
  | Mod
  | Or
  | And
  | Xor
  | LT
  | GT
  | LTE
  | GTE
  | EQ
  | NEQ
  deriving (Show, Eq, Ord, Data)

type family XRet a
type family XBlock a
type family XBreak a
type family XIf a
type family XWhile a
type family XLet a
type family XLit a
type family XVar a
type family XBinOp a

data ExpX a
  = RetX (XRet a) (Maybe (ExpX a))
  | BlockX (XBlock a) [ExpX a]
  | BreakX (XBreak a) (Maybe (ExpX a))
  | IfX (XIf a) (ExpX a) (ExpX a) (Maybe (ExpX a))
  | WhileX (XWhile a) (ExpX a) (ExpX a)
  | LetX (XLet a) Ident (ExpX a)
  | LitX (XLit a) (LitX a)
  | VarX (XVar a) Ident
  | BinOpX (XBinOp a) (ExpX a) ArithOp (ExpX a)

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
