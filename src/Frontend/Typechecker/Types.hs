{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Typechecker.Types where

import Types
import Data.Data (Data)
import Relude

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
type instance XAss Tc = () 
type instance XSExp Tc = ()

type instance XLit Tc = TypeTc
type instance XVar Tc = TypeTc
type instance XBinOp Tc = TypeTc
type instance XExprStmt Tc = ()
type instance XApp Tc = TypeTc
type instance XExpr Tc = ()

type instance XIntLit Tc = ()
type instance XDoubleLit Tc = () 
type instance XStringLit Tc = () 
type instance XCharLit Tc = () 
type instance XBoolLit Tc = () 

type instance XInt Tc = ()
type instance XDouble Tc = ()
type instance XString Tc = ()
type instance XChar Tc = ()
type instance XBool Tc = ()
type instance XUnit Tc = ()
type instance XTyVar Tc = ()
type instance XTyFun Tc = ()
type instance XType Tc = MetaTy

newtype MetaTy = Meta Int
    deriving (Show)
