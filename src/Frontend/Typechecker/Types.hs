{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Types where

import Data.Data (Data)
import Relude hiding (Any)
import Frontend.Types
import Control.Lens (makeLenses)
import Frontend.Renamer.Types (Boundedness)

data Tc deriving (Data)

type ProgramTc = ProgramX Tc
type DefTc = DefX Tc
type FnTc = FnX Tc
type AdtTc = AdtX Tc
type ConstructorTc = ConstructorX Tc
type ArgTc = ArgX Tc
type ExprTc = ExprX Tc
type TypeTc = TypeX Tc
type LitTc = LitX Tc
type StmtTc = StmtX Tc
type BlockTc = BlockX Tc
type LamArgTc = LamArgX Tc
type MatchArmTc = MatchArmX Tc
type PatternTc = PatternX Tc

type TcInfo = (SourceInfo, TypeTc)
type TcInfoBound = (SourceInfo, TypeTc, Boundedness)

deriving instance Data StmtTc
deriving instance Data BlockTc
deriving instance Data ExprTc
deriving instance Data LitTc
deriving instance Data TypeTc
deriving instance Data LamArgTc
deriving instance Data MatchArmTc
deriving instance Data PatternTc

type instance XProgram Tc = NoExtField

type instance XArg Tc = NoExtField

type instance XDef Tc = DataConCantHappen
type instance XFn Tc = NoExtField

type instance XAdt Tc = SourceInfo
type instance XConstructor Tc = DataConCantHappen
type instance XEnumCons Tc = TcInfo
type instance XFunCons Tc = TcInfo

type instance XBlock Tc = TcInfo

type instance XStmt Tc = DataConCantHappen
type instance XRet Tc = TcInfo
type instance XEBlock Tc = NoExtField
type instance XBreak Tc = TcInfo
type instance XIf Tc = TcInfo
type instance XWhile Tc = TcInfo
type instance XLet Tc = StmtType
type instance XAss Tc = (StmtType, Boundedness)
type instance XSExp Tc = NoExtField

type instance XMatchArm Tc = SourceInfo
type instance XMatch Tc = TcInfo

type instance XPVar Tc = TcInfo
type instance XPEnumCon Tc = TcInfo
type instance XPFunCon Tc = TcInfo

type instance XLit Tc = TcInfo
type instance XVar Tc = TcInfoBound
type instance XPrefix Tc = TcInfo
type instance XBinOp Tc = TcInfo
type instance XExprStmt Tc = NoExtField
type instance XApp Tc = TcInfo
type instance XExpr Tc = DataConCantHappen

type instance XIntLit Tc = NoExtField
type instance XDoubleLit Tc = NoExtField
type instance XStringLit Tc = NoExtField
type instance XCharLit Tc = NoExtField
type instance XBoolLit Tc = NoExtField
type instance XUnitLit Tc = NoExtField

type instance XTyLit Tc = NoExtField
type instance XTyFun Tc = NoExtField
type instance XTyCon Tc = NoExtField
type instance XType Tc = MetaTy

type instance XLoop Tc = TcInfo
type instance XLam Tc = TcInfo
type instance XLamArg Tc = TypeTc

deriving instance Eq TypeTc
deriving instance Ord TypeTc

pattern Any :: (XType a ~ MetaTy) => TypeX a
pattern Any <- TypeX AnyX
    where
        Any = TypeX AnyX

data MetaTy = AnyX
    deriving (Show, Eq, Ord, Data)

data StmtType = StmtType { _stmtType :: TypeTc, _varType :: TypeTc, _stmtInfo :: SourceInfo}
    deriving (Show, Eq, Ord, Data, Typeable)
$(makeLenses ''StmtType)

pattern Int :: (XTyLit a ~ NoExtField) => TypeX a
pattern Int <- TyLitX NoExtField IntX
    where
        Int = TyLitX NoExtField IntX

pattern Double :: (XTyLit a ~ NoExtField) => TypeX a
pattern Double <- TyLitX NoExtField DoubleX
    where
        Double = TyLitX NoExtField DoubleX

pattern String :: (XTyLit a ~ NoExtField) => TypeX a
pattern String <- TyLitX NoExtField StringX
    where
        String = TyLitX NoExtField StringX

pattern Char :: (XTyLit a ~ NoExtField) => TypeX a
pattern Char <- TyLitX NoExtField CharX
    where
        Char = TyLitX NoExtField CharX

pattern Unit :: (XTyLit a ~ NoExtField) => TypeX a
pattern Unit <- TyLitX NoExtField UnitX
    where
        Unit = TyLitX NoExtField UnitX

pattern Bool :: (XTyLit a ~ NoExtField) => TypeX a
pattern Bool <- TyLitX NoExtField BoolX
    where
        Bool = TyLitX NoExtField BoolX
