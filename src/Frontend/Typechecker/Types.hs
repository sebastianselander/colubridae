{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Types where

import Data.Data (Data)
import Relude
import Types
import Control.Lens (makeLenses)
import Control.Lens.Getter (view)

data Tc deriving (Data)

type ProgramTc = ProgramX Tc
type DefTc = DefX Tc
type ArgTc = ArgX Tc
type ExprTc = ExprX Tc
type TypeTc = TypeX Tc
type LitTc = LitX Tc
type StmtTc = StmtX Tc
type BlockTc = BlockX Tc
type SugarStmtTc = SugarStmtX Tc

type TcInfo = (SourceInfo, TypeTc)

deriving instance Data StmtTc
deriving instance Data SugarStmtTc
deriving instance Data BlockTc
deriving instance Data ExprTc
deriving instance Data LitTc
deriving instance Data TypeTc

type instance XProgram Tc = NoExtField

type instance XArg Tc = NoExtField

type instance XDef Tc = NoExtField

type instance XBlock Tc = TcInfo

type instance XStmt Tc = SugarStmtTc
type instance XRet Tc = TcInfo
type instance XSBlock Tc = NoExtField
type instance XBreak Tc = TcInfo
type instance XIf Tc = TcInfo
type instance XWhile Tc = TcInfo
type instance XLet Tc = StmtType
type instance XAss Tc = StmtType
type instance XSExp Tc = NoExtField

type instance XLit Tc = TcInfo
type instance XVar Tc = TcInfo
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
type instance XType Tc = MetaTy

type instance XLoop Tc = TcInfo

deriving instance Eq TypeTc
deriving instance Ord TypeTc

pattern Unsolvable :: (XType a ~ MetaTy) => TypeX a
pattern Unsolvable <- TypeX UnsolvableX
    where
        Unsolvable = TypeX UnsolvableX

pattern Mut :: (XType a ~ MetaTy) => TypeTc -> TypeX a
pattern Mut ty <- TypeX (MutableX ty)
  where
    Mut ty = TypeX (MutableX ty)

data MetaTy = MutableX TypeTc | UnsolvableX
    deriving (Show, Eq, Ord, Data)

data StmtType = StmtType { _stmtType :: TypeTc, _varType :: TypeTc, _stmtInfo :: SourceInfo}
    deriving (Show, Eq, Ord, Data, Typeable)
$(makeLenses ''StmtType)

instance Pretty StmtType where
  pPretty ty = pPretty $ view varType ty

instance Pretty MetaTy where
    pPretty = \case
        UnsolvableX -> "#Unsolvable#"
        MutableX ty -> pPretty ty <> "?"

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
