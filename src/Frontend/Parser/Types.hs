{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Parser.Types where

import Data.Data (Data)
import Relude hiding (Type, intercalate, replicate)

import Frontend.Types
import Names (Ident, Namespace)


data Par
    deriving (Data)


type ExprPar = Expr Ident Par


type ProgramPar = Program Ident Par


type ImportPar = Import Ident Par


type LitPar = Lit Par


type ArgPar = Arg Ident Par


type DefPar = Def Ident Par


type FnPar = Fn Ident Par


type AdtPar = Adt Ident Par


type ConstructorPar = Constructor Ident Par


type TypePar = Type Ident Par


type BlockPar = Block Ident Par


type StmtPar = Stmt Ident Par


type LamArgPar = LamArg Ident Par


type MatchArmPar = MatchArm Ident Par


type PatternPar = Pattern Ident Par


deriving instance Data ExprPar


deriving instance Data ProgramPar


deriving instance Data LitPar


deriving instance Data ArgPar


deriving instance Data DefPar


deriving instance Data ImportPar


deriving instance Data FnPar


deriving instance Data AdtPar


deriving instance Data ConstructorPar


deriving instance Data TypePar


deriving instance Data StmtPar


deriving instance Data BlockPar


deriving instance Data MatchArmPar


deriving instance Data PatternPar


type instance XProgram Par = Namespace


data ExtraImports a
    = ImportAs Namespace Ident a
    | ImportQualified Namespace a
    deriving (Data, Eq, Ord, Show)


type instance XImport Par = ExtraImports SourceInfo


type instance XImportExplicit Par = SourceInfo


type instance XArg Par = SourceInfo


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


type instance XVar Par = (SourceInfo, Maybe Namespace)


type instance XPrefix Par = SourceInfo


type instance XBinOp Par = SourceInfo


type instance XExprStmt Par = SourceInfo


type instance XApp Par = SourceInfo


type instance XRet Par = SourceInfo


type instance XEBlock Par = NoExtField


type instance XBreak Par = SourceInfo


type instance XIf Par = SourceInfo


type instance XWhile Par = SourceInfo


type instance XLet Par = (SourceInfo, Maybe TypePar)


type instance XAss Par = SourceInfo


type instance XExpr Par = DataConCantHappen


type instance XMatchArm Par = SourceInfo


type instance XMatch Par = SourceInfo


type instance XPVar Par = SourceInfo


type instance XPEnumCon Par = SourceInfo


type instance XPFunCon Par = SourceInfo


type instance XIntLit Par = NoExtField


type instance XDoubleLit Par = NoExtField


type instance XStringLit Par = NoExtField


type instance XCharLit Par = NoExtField


type instance XBoolLit Par = NoExtField


type instance XUnitLit Par = NoExtField


type instance XTyLit Par = SourceInfo


type instance XTyFun Par = SourceInfo


type instance XType Par = UnresolvedType


type instance XTyCon Par = SourceInfo


type instance XTypeVar Par = SourceInfo


type instance XLoop Par = SourceInfo


type instance XLam Par = SourceInfo


type instance XLamArg Par = (SourceInfo, Maybe TypePar)


deriving instance Data LamArgPar
