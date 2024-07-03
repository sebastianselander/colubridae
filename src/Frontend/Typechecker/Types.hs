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
import Control.Lens.Getter (view)
import Frontend.Renamer.Types (Boundedness)

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
type LamArgTc = LamArgX Tc

type TcInfo = (SourceInfo, TypeTc)
type TcInfoBound = (SourceInfo, TypeTc, Boundedness)

deriving instance Data StmtTc
deriving instance Data SugarStmtTc
deriving instance Data BlockTc
deriving instance Data ExprTc
deriving instance Data LitTc
deriving instance Data TypeTc
deriving instance Data LamArgTc

type instance XProgram Tc = NoExtField

type instance XArg Tc = NoExtField

type instance XDef Tc = NoExtField

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

type instance XLit Tc = TcInfo
type instance XVar Tc = TcInfoBound
type instance XPrefix Tc = TcInfo
type instance XBinOp Tc = TcInfo
type instance XExprStmt Tc = NoExtField
type instance XApp Tc = TcInfo
type instance XExpr Tc = SugarStmtTc

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
type instance XLam Tc = TcInfo
type instance XLamArg Tc = TypeTc

deriving instance Eq TypeTc
deriving instance Ord TypeTc

pattern Any :: (XType a ~ MetaTy) => TypeX a
pattern Any <- TypeX AnyX
    where
        Any = TypeX AnyX

pattern Mut :: (XType a ~ MetaTy) => TypeTc -> TypeX a
pattern Mut ty <- TypeX (MutableX ty)
  where
    Mut ty = TypeX (MutableX ty)

pattern Meta :: (XType a ~ MetaTy) => Int -> TypeX a
pattern Meta n <- TypeX (MetaTyVar n)
    where
      Meta n = TypeX (MetaTyVar n)
 
data MetaTy = MutableX TypeTc | MetaTyVar Int | AnyX
    deriving (Show, Eq, Ord, Data)

data StmtType = StmtType { _stmtType :: TypeTc, _varType :: TypeTc, _stmtInfo :: SourceInfo}
    deriving (Show, Eq, Ord, Data, Typeable)
$(makeLenses ''StmtType)

instance Pretty StmtType where
  pPretty ty = pPretty $ view varType ty

instance Pretty MetaTy where
    pPretty = \case
        AnyX -> "any"
        MutableX ty -> pPretty ty <> "?"
        MetaTyVar n -> "#a" <> show n <> "#"

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
