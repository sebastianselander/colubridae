{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Typechecker.Types where

import Data.Data (Data)
import Data.List ((!!))
import Data.Text (pack)
import Relude
import Types

data Tc deriving (Data)

type ProgramTc = ProgramX Tc
type DefTc = DefX Tc
type ArgTc = ArgX Tc
type ExprTc = ExprX Tc
type TypeTc = TypeX Tc
type LitTc = LitX Tc
type StmtTc = StmtX Tc
type BlockTc = BlockX Tc

type instance XProgram Tc = ()

type instance XArg Tc = ()

type instance XDef Tc = ()

type instance XBlock Tc = TypeTc

type instance XStmt Tc = SugarStmtX Tc
type instance XRet Tc = TypeTc
type instance XSBlock Tc = ()
type instance XBreak Tc = TypeTc
type instance XIf Tc = TypeTc
type instance XWhile Tc = TypeTc
type instance XLet Tc = (TypeTc, TypeTc)
type instance XAss Tc = (TypeTc, TypeTc)
type instance XSExp Tc = ()

type instance XLit Tc = TypeTc
type instance XVar Tc = TypeTc
type instance XBinOp Tc = TypeTc
type instance XExprStmt Tc = ()
type instance XApp Tc = TypeTc
type instance XExpr Tc = Void

type instance XIntLit Tc = ()
type instance XDoubleLit Tc = ()
type instance XStringLit Tc = ()
type instance XCharLit Tc = ()
type instance XBoolLit Tc = ()
type instance XUnitLit Tc = ()

type instance XTyLit Tc = ()
type instance XTyVar Tc = ()
type instance XTyFun Tc = ()
type instance XType Tc = MetaTy

type instance XLoop Tc = TypeTc

deriving instance Eq TypeTc
deriving instance Ord TypeTc

pattern Meta :: (XType a ~ MetaTy) => Int -> TypeX a
pattern Meta n <- TypeX (MetaX n)
    where
        Meta n = TypeX $ MetaX n

pattern Unsolvable :: (XType a ~ MetaTy) => TypeX a
pattern Unsolvable <- TypeX UnsolvableX
    where
        Unsolvable = TypeX UnsolvableX

pattern Mut :: (XType a ~ MetaTy) => TypeTc -> TypeX a
pattern Mut ty <- TypeX (MutableX ty)
  where
    Mut ty = TypeX (MutableX ty)

data MetaTy = MutableX TypeTc | MetaX Int | UnsolvableX
    deriving (Show, Eq, Ord)

instance Pretty MetaTy where
    pPretty = \case
        MetaX n -> "#" <> (letters !! n) <> "#"
        UnsolvableX -> "#Unsolvable#"
        MutableX ty -> pPretty ty <> "?"

letters :: [Text]
letters = fmap pack $ [1 ..] >>= flip replicateM ['a' .. 'z']

pattern Int :: (XTyLit a ~ ()) => TypeX a
pattern Int <- TyLitX () IntX
    where
        Int = TyLitX () IntX

pattern Double :: (XTyLit a ~ ()) => TypeX a
pattern Double <- TyLitX () DoubleX
    where
        Double = TyLitX () DoubleX

pattern String :: (XTyLit a ~ ()) => TypeX a
pattern String <- TyLitX () StringX
    where
        String = TyLitX () StringX

pattern Char :: (XTyLit a ~ ()) => TypeX a
pattern Char <- TyLitX () CharX
    where
        Char = TyLitX () CharX

pattern Unit :: (XTyLit a ~ ()) => TypeX a
pattern Unit <- TyLitX () UnitX
    where
        Unit = TyLitX () UnitX

pattern Bool :: (XTyLit a ~ ()) => TypeX a
pattern Bool <- TyLitX () BoolX
    where
        Bool = TyLitX () BoolX
