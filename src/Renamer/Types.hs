{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Renamer.Types where

import Relude
import Data.Data (Data)
import Types
import Parser.Types


data Boundedness = Free | Bound
  deriving (Show, Eq, Ord, Data)

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

type instance XStmt Rn = XStmt Par

type instance XRet Rn = XRet Par
type instance XBlock Rn = XBlock Par
type instance XBreak Rn = XBreak Par
type instance XIf Rn = XIf Par
type instance XWhile Rn = XWhile Par
type instance XLet Rn = XLet Par
type instance XSExp Rn = XSExp Par

type instance XLit Rn = XLit Par
type instance XVar Rn = (XVar Par, Boundedness)
type instance XBinOp Rn = XBinOp Par
type instance XExprStmt Rn = XExprStmt Par
type instance XApp Rn = XApp Par
type instance XExpr Rn = XExpr Par

type instance XIntLit Rn = XIntLit Par
type instance XDoubleLit Rn = XDoubleLit Par 
type instance XStringLit Rn = XStringLit Par 
type instance XCharLit Rn = XCharLit Par 
type instance XBoolLit Rn = XBoolLit Par 

type instance XInt Rn = XInt Par
type instance XDouble Rn = XDouble Par
type instance XString Rn = XString Par
type instance XChar Rn = XChar Par
type instance XBool Rn = XBool Par
type instance XUnit Rn = XUnit Par
type instance XTyVar Rn = XTyVar Par
type instance XTyFun Rn = XTyFun Par

pattern FreeVar :: SourceInfo -> Ident -> ExprRn
pattern FreeVar pos name <- VarX (pos, Free) name
  where FreeVar pos name = VarX (pos, Free) name

pattern BoundVar :: SourceInfo -> Ident -> ExprRn
pattern BoundVar pos name <- VarX (pos, Bound) name
  where BoundVar pos name = VarX (pos, Bound) name
