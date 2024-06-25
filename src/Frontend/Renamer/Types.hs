{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Renamer.Types where

import Data.Data (Data)
import Frontend.Parser.Types
import Relude
import Types

data Boundedness = Free | Bound | Toplevel | Lambda
    deriving (Show, Eq, Ord, Data)

instance Pretty Boundedness where
    pPretty = show

data Rn
    deriving (Data)

type ProgramRn = ProgramX Rn
type DefRn = DefX Rn
type ArgRn = ArgX Rn
type ExprRn = ExprX Rn
type TypeRn = TypeX Rn
type LitRn = LitX Rn
type StmtRn = StmtX Rn
type BlockRn = BlockX Rn
type SugarStmtRn = SugarStmtX Rn

deriving instance Data ExprRn
deriving instance Data ProgramRn
deriving instance Data LitRn
deriving instance Data ArgRn
deriving instance Data DefRn
deriving instance Data TypeRn
deriving instance Data BlockRn
deriving instance Data StmtRn
deriving instance Data SugarStmtRn

type instance XProgram Rn = XProgram Par

type instance XArg Rn = XArg Par

type instance XDef Rn = XDef Par

type instance XBlock Rn = XBlock Par

type instance XRet Rn = XRet Par
type instance XBreak Rn = XBreak Par
type instance XSBlock Rn = NoExtField
type instance XIf Rn = XIf Par
type instance XWhile Rn = XWhile Par
type instance XLet Rn = (SourceInfo, Mutability, Maybe TypeRn)
type instance XAss Rn = (SourceInfo, Boundedness)
type instance XSExp Rn = XSExp Par
type instance XStmt Rn = SugarStmtX Rn

type instance XLit Rn = XLit Par
type instance XVar Rn = (SourceInfo, Boundedness)
type instance XBinOp Rn = XBinOp Par
type instance XExprStmt Rn = XExprStmt Par
type instance XApp Rn = XApp Par
type instance XExpr Rn = DataConCantHappen

type instance XIntLit Rn = NoExtField
type instance XDoubleLit Rn = NoExtField
type instance XStringLit Rn = NoExtField
type instance XCharLit Rn = NoExtField
type instance XBoolLit Rn = NoExtField
type instance XUnitLit Rn = NoExtField

type instance XTyLit Rn = NoExtField
type instance XTyVar Rn = NoExtField
type instance XTyFun Rn = NoExtField
type instance XType Rn = DataConCantHappen

type instance XLoop Rn = SourceInfo
