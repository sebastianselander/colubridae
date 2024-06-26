{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Parser.Types where

import Data.Data (Data)
import Relude hiding (intercalate, replicate)
import Types

data Par
    deriving (Data)

type ExprPar = ExprX Par
type ProgramPar = ProgramX Par
type LitPar = LitX Par
type ArgPar = ArgX Par
type DefPar = DefX Par
type TypePar = TypeX Par
type BlockPar = BlockX Par
type StmtPar = StmtX Par

deriving instance Data ExprPar
deriving instance Data ProgramPar
deriving instance Data LitPar
deriving instance Data ArgPar
deriving instance Data DefPar
deriving instance Data TypePar
deriving instance Data StmtPar
deriving instance Data BlockPar

type instance XProgram Par = NoExtField

type instance XArg Par = (SourceInfo, Mutability)

type instance XDef Par = SourceInfo

type instance XBlock Par = SourceInfo

type instance XStmt Par = SugarStmtX Par
type instance XRet Par = SourceInfo
type instance XSBlock Par = NoExtField
type instance XBreak Par = SourceInfo
type instance XIf Par = SourceInfo
type instance XWhile Par = SourceInfo
type instance XLet Par = (SourceInfo, Mutability, Maybe TypePar)
type instance XAss Par = SourceInfo
type instance XSExp Par = NoExtField

type instance XLit Par = SourceInfo
type instance XVar Par = SourceInfo
type instance XBinOp Par = SourceInfo
type instance XExprStmt Par = SourceInfo
type instance XApp Par = SourceInfo
type instance XExpr Par = DataConCantHappen

type instance XIntLit Par = NoExtField
type instance XDoubleLit Par = NoExtField
type instance XStringLit Par = NoExtField
type instance XCharLit Par = NoExtField
type instance XBoolLit Par = NoExtField
type instance XUnitLit Par = NoExtField

type instance XTyLit Par = NoExtField
type instance XTyFun Par = NoExtField
type instance XType Par = DataConCantHappen

type instance XLoop Par = SourceInfo

deriving instance Data (SugarStmtX Par)
