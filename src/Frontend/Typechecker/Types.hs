{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Typechecker.Types where

import Types
import Data.Data (Data)
import Relude
import Data.List ((!!))
import Data.Text (pack)

data Tc deriving (Data)

type ProgramTc = ProgramX Tc
type DefTc = DefX Tc
type ArgTc = ArgX Tc
type ExprTc = ExprX Tc
type TypeTc = TypeX Tc
type LitTc = LitX Tc
type StmtTc = StmtX Tc

type instance XProgram Tc = ()

type instance XArg Tc = ()

type instance XDef Tc = ()

type instance XStmt Tc = ()

type instance XRet Tc = TypeTc
type instance XBlock Tc = TypeTc
type instance XBreak Tc = TypeTc
type instance XIf Tc = TypeTc
type instance XWhile Tc = TypeTc
type instance XLet Tc = (Mutability, TypeTc) 
type instance XAss Tc = TypeTc
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

type instance XTyLit Tc = ()
type instance XTyVar Tc = ()
type instance XTyFun Tc = ()
type instance XType Tc = MetaTy

deriving instance Eq TypeTc

data MetaTy = Meta Int | Unsolvable
   deriving (Show, Ord, Eq)

instance Pretty MetaTy where
  pPretty = \case
    Meta n -> "#" <> (letters !! n) <> "#"
    Unsolvable -> "#Unsolvable#"
        
letters :: [Text]
letters = fmap pack $ [1 ..] >>= flip replicateM ['a' .. 'z']

pattern Int :: (XTyLit a ~ ()) => TypeX a
pattern Int <- TyLitX () IntX
  where Int = TyLitX () IntX

pattern Double :: (XTyLit a ~ ()) => TypeX a
pattern Double <- TyLitX () DoubleX
  where Double = TyLitX () DoubleX

pattern String :: (XTyLit a ~ ()) => TypeX a
pattern String <- TyLitX () StringX
  where String = TyLitX () StringX

pattern Char :: (XTyLit a ~ ()) => TypeX a
pattern Char <- TyLitX () CharX
  where Char = TyLitX () CharX

pattern Unit :: (XTyLit a ~ ()) => TypeX a
pattern Unit <- TyLitX () UnitX
  where Unit = TyLitX () UnitX

pattern Bool :: (XTyLit a ~ ()) => TypeX a
pattern Bool <- TyLitX () BoolX
  where Bool = TyLitX () BoolX
