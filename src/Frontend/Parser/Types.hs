{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Parser.Types where

import Data.Data (Data)
import Relude hiding (intercalate, replicate)
import Frontend.Types
import Error.Diagnose (Position(..))

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
type MatchArmPar = MatchArmX Par
type PatternPar = PatternX Par

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
deriving instance Data MatchArmPar
deriving instance Data PatternPar

type instance XProgram Par = NoExtField

type instance XArg Par = Position

type instance XDef Par = DataConCantHappen
type instance XFn Par = Position

type instance XAdt Par = Position
type instance XFunCons Par = Position
type instance XEnumCons Par = Position
type instance XConstructor Par = DataConCantHappen

type instance XBlock Par = Position

type instance XStmt Par = DataConCantHappen
type instance XSExp Par = NoExtField

type instance XLit Par = Position
type instance XVar Par = Position
type instance XPrefix Par = Position
type instance XBinOp Par = Position
type instance XExprStmt Par = Position
type instance XApp Par = Position
type instance XRet Par = Position
type instance XEBlock Par = NoExtField
type instance XBreak Par = Position
type instance XIf Par = Position
type instance XWhile Par = Position
type instance XLet Par = (Position, Maybe TypePar)
type instance XAss Par = Position
type instance XExpr Par = DataConCantHappen

type instance XMatchArm Par = Position
type instance XMatch Par = Position

type instance XPVar Par = Position
type instance XPEnumCon Par = Position
type instance XPFunCon Par = Position

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

type instance XLoop Par = Position
type instance XLam Par = Position
type instance XLamArg Par = (Position, Maybe TypePar)

deriving instance Data (LamArgX Par)
    
deriving instance Data Position
