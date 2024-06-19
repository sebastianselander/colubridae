{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Renamer.Types where

import Data.Data (Data)
import Frontend.Parser.Types
import Relude
import Types

data Boundedness = Free | Bound
  deriving (Show, Eq, Ord, Data)

instance Pretty Boundedness where
  pPretty Free = "Free"
  pPretty Bound = "Bound"
    
data Rn
  deriving (Data)

type ProgramRn = ProgramX Rn
type DefRn = DefX Rn
type ArgRn = ArgX Rn
type ExprRn = ExprX Rn
type TypeRn = TypeX Rn
type LitRn = LitX Rn
type StmtRn = StmtX Rn

type instance XProgram Rn = XProgram Par

type instance XArg Rn = XArg Par

type instance XDef Rn = XDef Par


type instance XRet Rn = XRet Par
type instance XBlock Rn = XBlock Par
type instance XBreak Rn = XBreak Par
type instance XIf Rn = XIf Par
type instance XWhile Rn = XWhile Par
type instance XLet Rn = XLet Par
type instance XAss Rn = Void
type instance XSExp Rn = XSExp Par
type instance XStmt Rn = AssRn

data AssRn = AssRn SourceInfo VarRn ExprRn
    deriving (Show)

instance Pretty AssRn where
  pPretty (AssRn _ var expr) = unwords [pPretty var, "=", pPretty expr]

type instance XLit Rn = XLit Par
type instance XVar Rn = Void
type instance XBinOp Rn = XBinOp Par
type instance XExprStmt Rn = XExprStmt Par
type instance XApp Rn = XApp Par
type instance XExpr Rn = (SourceInfo, VarRn)

data VarRn
  = BoundVar Ident
  | FreeVar Ident
  | ToplevelVar Ident
  | LambdaVar Ident
    deriving (Show)
instance Pretty (SourceInfo, VarRn) where
  pPretty (_, var) = pPretty var

instance Pretty VarRn where
  pPretty = \case
    BoundVar ident -> pPretty ident
    FreeVar ident -> pPretty ident
    ToplevelVar ident -> pPretty ident
    LambdaVar ident -> pPretty ident

type instance XIntLit Rn = XIntLit Par
type instance XDoubleLit Rn = XDoubleLit Par
type instance XStringLit Rn = XStringLit Par
type instance XCharLit Rn = XCharLit Par
type instance XBoolLit Rn = XBoolLit Par

type instance XTyLit Rn = XTyLit Par
type instance XTyVar Rn = XTyVar Par
type instance XTyFun Rn = XTyFun Par
type instance XType Rn = XType Par
