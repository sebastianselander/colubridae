{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Parser.Types where

import Data.Data (Data)
import Relude hiding (intercalate, replicate)
import Frontend.Types

data Par
    deriving (Data)

type ExprPar = ExprX Par
type ProgramPar = ProgramX Par
type LitPar = LitX Par
type ArgPar = ArgX Par
type DefPar = DefX Par
type FnPar = FnX Par
type AdtPar = AdtX Par
type ConstructorPar = ConstructorX Par
type TypePar = TypeX Par
type BlockPar = BlockX Par
type StmtPar = StmtX Par
type LamArgPar = LamArgX Par

deriving instance Data ExprPar
deriving instance Data ProgramPar
deriving instance Data LitPar
deriving instance Data ArgPar
deriving instance Data DefPar
deriving instance Data FnPar
deriving instance Data AdtPar
deriving instance Data ConstructorPar
deriving instance Data TypePar
deriving instance Data StmtPar
deriving instance Data BlockPar

type instance XProgram Par = NoExtField

type instance XArg Par = (SourceInfo, Mutability)

type instance XDef Par = DataConCantHappen
type instance XFn Par = SourceInfo

type instance XAdt Par = SourceInfo
type instance XFunCons Par = SourceInfo
type instance XEnumCons Par = SourceInfo
type instance XConstructor Par = DataConCantHappen

type instance XBlock Par = SourceInfo

type instance XStmt Par = DataConCantHappen
type instance XSExp Par = NoExtField

type instance XLit Par = SourceInfo
type instance XVar Par = SourceInfo
type instance XPrefix Par = SourceInfo
type instance XBinOp Par = SourceInfo
type instance XExprStmt Par = SourceInfo
type instance XApp Par = SourceInfo
type instance XRet Par = SourceInfo
type instance XEBlock Par = NoExtField
type instance XBreak Par = SourceInfo
type instance XIf Par = SourceInfo
type instance XWhile Par = SourceInfo
type instance XLet Par = (SourceInfo, Mutability, Maybe TypePar)
type instance XAss Par = SourceInfo
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
type instance XTyCon Par = NoExtField

type instance XLoop Par = SourceInfo
type instance XLam Par = SourceInfo
type instance XLamArg Par = (SourceInfo, Mutability, Maybe TypePar)

deriving instance Data (LamArgX Par)
    
