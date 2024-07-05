{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Types where

import Data.Data (Data)
import Data.Kind qualified
import Data.Tuple.Extra (both)
import GHC.Show (show)
import Names
import Relude hiding (Type, concat, intercalate, replicate)
import Relude qualified
import Text.Megaparsec (Pos)
import Text.Megaparsec.Pos (unPos)
import Prettyprinter qualified as Pretty
import Prettyprinter (Pretty)

data NoExtField = NoExtField
    deriving (Show, Eq, Ord, Data, Typeable, Generic)

data DataConCantHappen
    deriving (Show, Eq, Ord, Data, Typeable, Generic)

data Mutability = Mutable | Immutable
    deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty Mutability where
    pretty Mutable = "mut"
    pretty Immutable = Pretty.emptyDoc

data Span = Span
    { start :: !(Pos, Pos)
    , end :: !(Pos, Pos)
    }
    deriving (Eq, Ord, Data)

instance Show Span where
    show Span {start, end} =
        let (bl, bc) = both (Relude.show . unPos) start
            (al, ac) = both (Relude.show . unPos) end
         in Relude.concat ["(", bl, ":", bc, "->", al, ":", ac, ")"]

data SourceInfo = SourceInfo
    { spanInfo :: !(Maybe Span)
    , sourceFile :: !FilePath
    }
    deriving (Eq, Ord, Data)

instance Show SourceInfo where
    show info = maybe "no pos" Relude.show info.spanInfo

-- Program
data ProgramX a = ProgramX !(XProgram a) [DefX a]
type family XProgram a

deriving instance (ForallX Show a) => Show (ProgramX a)
deriving instance (ForallX Typeable a) => Typeable (ProgramX a)

-- Definition
data DefX a
    = Fn !(XDef a) Ident [ArgX a] (TypeX a) (BlockX a)
type family XDef a

deriving instance (ForallX Show a) => Show (DefX a)
deriving instance (ForallX Typeable a) => Typeable (DefX a)

-- Argument
data ArgX a = ArgX !(XArg a) Ident (TypeX a)
type family XArg a

deriving instance (ForallX Show a) => Show (ArgX a)
deriving instance (ForallX Typeable a) => Typeable (ArgX a)

-- Type
data TypeX a
    = TyLitX !(XTyLit a) TyLit
    | TyFunX !(XTyFun a) [TypeX a] (TypeX a)
    | TypeX !(XType a)
type family XTyLit a
type family XTyFun a
type family XType a

coerceType ::
    (XTyLit t1 ~ XTyLit t2, XTyFun t1 ~ XTyFun t2, XType t1 ~ XType t2) => TypeX t1 -> TypeX t2
coerceType ty = case ty of
    TyLitX a b -> TyLitX a b
    TyFunX a b c -> TyFunX a (fmap coerceType b) (coerceType c)
    TypeX a -> TypeX a

data TyLit = UnitX | StringX | IntX | DoubleX | CharX | BoolX
    deriving (Show, Eq, Ord, Enum, Data)

deriving instance (ForallX Show a) => Show (TypeX a)
deriving instance (ForallX Typeable a) => Typeable (TypeX a)

data BlockX a = BlockX !(XBlock a) [StmtX a] (Maybe (ExprX a))
type family XBlock a

deriving instance (ForallX Show a) => Show (BlockX a)
deriving instance (ForallX Typeable a) => Typeable (BlockX a)

-- Statement
data StmtX a
    = SExprX !(XSExp a) (ExprX a)
    | StmtX !(XStmt a)
type family XSExp a
type family XStmt a

data AssignOp
    = AddAssign
    | SubAssign
    | MulAssign
    | DivAssign
    | ModAssign
    | Assign
    deriving (Show, Eq, Ord, Data)

deriving instance (ForallX Show a) => Show (StmtX a)
deriving instance (ForallX Typeable a) => Typeable (StmtX a)

-- Expression
data ExprX a
    = LitX !(XLit a) (LitX a)
    | VarX !(XVar a) Ident
    | BinOpX !(XBinOp a) (ExprX a) BinOp (ExprX a)
    | PrefixX !(XPrefix a) PrefixOp (ExprX a)
    | AppX !(XApp a) (ExprX a) [ExprX a]
    | LetX !(XLet a) Ident (ExprX a)
    | AssX !(XAss a) Ident AssignOp (ExprX a)
    | RetX !(XRet a) (Maybe (ExprX a))
    | EBlockX !(XEBlock a) (BlockX a)
    | BreakX !(XBreak a) (Maybe (ExprX a))
    | IfX !(XIf a) (ExprX a) (BlockX a) (Maybe (BlockX a))
    | WhileX !(XWhile a) (ExprX a) (BlockX a)
    | LoopX !(XLoop a) (BlockX a)
    | LamX !(XLam a) [LamArgX a] (ExprX a)
    | ExprX !(XExpr a)
type family XExprStmt a
type family XLit a
type family XVar a
type family XPrefix a
type family XBinOp a
type family XApp a
type family XAss a
type family XLet a
type family XRet a
type family XEBlock a
type family XBreak a
type family XIf a
type family XWhile a
type family XExpr a

data LamArgX a = LamArgX !(XLamArg a) Ident
type family XLamArg a

deriving instance (ForallX Show a) => Show (LamArgX a)
deriving instance (ForallX Typeable a) => Typeable (LamArgX a)

type family XLoop a
type family XLam a

deriving instance (ForallX Show a) => Show (ExprX a)
deriving instance (ForallX Typeable a) => Typeable (ExprX a)

data PrefixOp = Not | Neg
    deriving (Show, Eq, Ord, Data)

data BinOp
    = Mul
    | Div
    | Add
    | Sub
    | Mod
    | Or
    | And
    | Lt
    | Gt
    | Lte
    | Gte
    | Eq
    | Neq
    deriving (Show, Eq, Ord, Data)

-- Literal
data LitX a
    = IntLitX !(XIntLit a) Integer
    | DoubleLitX !(XDoubleLit a) Double
    | StringLitX !(XStringLit a) Text
    | CharLitX !(XCharLit a) Char
    | BoolLitX !(XBoolLit a) Bool
    | UnitLitX !(XUnitLit a)
type family XIntLit a
type family XDoubleLit a
type family XStringLit a
type family XCharLit a
type family XBoolLit a
type family XUnitLit a

deriving instance (ForallX Show a) => Show (LitX a)
deriving instance (ForallX Typeable a) => Typeable (LitX a)

type ForallX (c :: Data.Kind.Type -> Constraint) a =
    ( c (XApp a)
    , c (XArg a)
    , c (XPrefix a)
    , c (XBinOp a)
    , c (XBlock a)
    , c (XBoolLit a)
    , c (XBreak a)
    , c (XCharLit a)
    , c (XUnitLit a)
    , c (XDef a)
    , c (XDoubleLit a)
    , c (XExprStmt a)
    , c (XIf a)
    , c (XIntLit a)
    , c (XLet a)
    , c (XAss a)
    , c (XLit a)
    , c (XProgram a)
    , c (XRet a)
    , c (XSExp a)
    , c (XStmt a)
    , c (XStringLit a)
    , c (XTyLit a)
    , c (XTyFun a)
    , c (XVar a)
    , c (XWhile a)
    , c (XEBlock a)
    , c (XExpr a)
    , c (XType a)
    , c (XLoop a)
    , c (XLam a)
    , c (XLamArg a)
    )
