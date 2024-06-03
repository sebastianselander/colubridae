{-# LANGUAGE TypeFamilies #-}

module Parser.Types where

import Relude hiding (Type)
import Types
import Parser.Utils

data Par

type ExprPar = ExpX Par
type ProgramPar = Program Par
type LitPar = LitX Par
type ArgPar = ArgX Par
type DefPar = DefX Par
type TypePar = TypeX Par

deriving instance Show ProgramPar
deriving instance Show DefPar
deriving instance Show ArgPar
deriving instance Show TypePar
deriving instance Show ExprPar
deriving instance Show LitPar

type instance XArg Par = SourceInfo

type instance XDef Par = SourceInfo

type instance XRet Par = SourceInfo
type instance XBlock Par = SourceInfo
type instance XBreak Par = SourceInfo
type instance XIf Par = SourceInfo
type instance XWhile Par = SourceInfo
type instance XLet Par = SourceInfo
type instance XLit Par = SourceInfo
type instance XVar Par = SourceInfo
type instance XBinOp Par = SourceInfo

type instance XIntLit Par = ()
type instance XDoubleLit Par = ()
type instance XStringLit Par = ()
type instance XCharLit Par = ()
type instance XBoolLit Par = ()

data IsHex = IsHex | NotIsHex
  deriving (Show, Eq, Ord)

type instance XInt Par = ()
type instance XDouble Par = ()
type instance XString Par = ()
type instance XChar Par = ()
type instance XBool Par = ()
type instance XUnit Par = ()
type instance XTyVar Par = ()
type instance XTyFun Par = ()
