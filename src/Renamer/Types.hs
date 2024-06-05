{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Renamer.Types where

import Relude
import Data.Data (Data)
import Types


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

type instance XProgram Rn = ()

type instance XArg Rn = SourceInfo

type instance XDef Rn = SourceInfo

type instance XStmt Rn = ()

type instance XRet Rn = ()
type instance XBlock Rn = ()
type instance XBreak Rn = ()
type instance XIf Rn = ()
type instance XWhile Rn = ()
type instance XLet Rn = Mutability
type instance XSExp Rn = ()

type instance XLit Rn = SourceInfo
type instance XVar Rn = (SourceInfo, Boundedness)
type instance XBinOp Rn = SourceInfo
type instance XExprStmt Rn = SourceInfo
type instance XApp Rn = SourceInfo

type instance XIntLit Rn = ()
type instance XDoubleLit Rn = ()
type instance XStringLit Rn = ()
type instance XCharLit Rn = ()
type instance XBoolLit Rn = ()

type instance XInt Rn = ()
type instance XDouble Rn = ()
type instance XString Rn = ()
type instance XChar Rn = ()
type instance XBool Rn = ()
type instance XUnit Rn = ()
type instance XTyVar Rn = ()
type instance XTyFun Rn = ()

pattern FreeVar :: SourceInfo -> Ident -> ExprRn
pattern FreeVar pos name <- VarX (pos, Free) name
  where FreeVar pos name = VarX (pos, Free) name

pattern BoundVar :: SourceInfo -> Ident -> ExprRn
pattern BoundVar pos name <- VarX (pos, Bound) name
  where BoundVar pos name = VarX (pos, Bound) name
