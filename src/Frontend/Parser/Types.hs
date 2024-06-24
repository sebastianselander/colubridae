{-# LANGUAGE PatternSynonyms #-}
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

type instance XProgram Par = ()

type instance XArg Par = (SourceInfo, Mutability)

type instance XDef Par = SourceInfo

type instance XBlock Par = SourceInfo

type instance XStmt Par = SugarStmtX Par
type instance XRet Par = SourceInfo
type instance XSBlock Par = ()
type instance XBreak Par = SourceInfo
type instance XIf Par = SourceInfo
type instance XWhile Par = SourceInfo
type instance XLet Par = (SourceInfo, Mutability, Maybe TypePar)
type instance XAss Par = SourceInfo
type instance XSExp Par = ()

type instance XLit Par = SourceInfo
type instance XVar Par = SourceInfo
type instance XBinOp Par = SourceInfo
type instance XExprStmt Par = SourceInfo
type instance XApp Par = SourceInfo
type instance XExpr Par = Void

type instance XIntLit Par = ()
type instance XDoubleLit Par = ()
type instance XStringLit Par = ()
type instance XCharLit Par = ()
type instance XBoolLit Par = ()
type instance XUnitLit Par = ()

type instance XTyLit Par = ()
type instance XTyVar Par = ()
type instance XTyFun Par = ()
type instance XType Par = Void

type instance XLoop Par = SourceInfo

deriving instance Data (SugarStmtX Par)
