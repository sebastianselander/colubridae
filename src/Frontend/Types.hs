{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Types where

import Data.Data (Data)
import Data.Tuple.Extra (both)
import GHC.Show (show)
import Prettyprinter (Pretty (pretty))
import Relude hiding (Type, concat, intercalate, replicate)
import Text.Megaparsec (Pos, mkPos)
import Text.Megaparsec.Pos (unPos)

import Data.Kind qualified
import Data.Map qualified as Map
import Relude qualified

import Names

data Boundedness = Free | Bound | Function | Constructor | Imported | Builtin
    deriving (Data, Eq, Ord, Show)

instance Pretty Boundedness where
    pretty = Relude.show

data NoExtField = NoExtField
    deriving (Data, Eq, Generic, Ord, Show)

data DataConCantHappen
    deriving (Data, Eq, Generic, Ord, Show)

data Span = Span
    { start :: !(Pos, Pos)
    , end :: !(Pos, Pos)
    }
    deriving (Data, Eq, Ord)

emptyInfo :: SourceInfo
emptyInfo = SourceInfo {sourceFile = "", spanInfo = emptySpan}

emptySpan :: Span
emptySpan = Span (mkPos 1, mkPos 1) (mkPos 1, mkPos 1)

instance Show Span where
    show Span {start, end} =
        let (bl, bc) = both (Relude.show . unPos) start
         in Relude.concat [bl, ":", bc]

data SourceInfo = SourceInfo
    { spanInfo :: !Span
    , sourceFile :: !FilePath
    }
    deriving (Data, Eq, Ord)

instance Show SourceInfo where
    show info = info.sourceFile <> ":" <> Relude.show info.spanInfo

-- Program
data Program name a = Program !(XProgram a) (NonEmpty (Module name a))

type family XProgram a

data Module name a = Module !(XModule a) [Def name a]

type family XModule a

deriving instance (Show name, Forall Show a) => Show (Program name a)

deriving instance (Show name, Forall Show a) => Show (Module name a)

-- Definition
data Def name a
    = DefImport (Import name a)
    | DefAdt (Adt name a)
    | DefFn (Fn name a)
    | DefX !(XDef a)

type family XDef a

deriving instance (Show name, Forall Show a) => Show (Def name a)

{-|
We don't know if type `A` is a type parameter or a concrete type when
parsing so we use this type as a placeholder and resolve it during renaming
-}
data UnresolvedType = UnresolvedType SourceInfo TyVar
    deriving (Data, Eq, Ord, Show)

data TyParamList = Params SourceInfo [TyVar]
    deriving (Data, Eq, Ord, Show)

emptyTyParamList :: TyParamList
emptyTyParamList = Params emptyInfo []

member :: TyVar -> TyParamList -> Bool
member tyvar (Params _ xs) = tyvar `elem` xs

toList :: TyParamList -> [TyVar]
toList (Params _ xs) = xs

toMap :: TyParamList -> Map TyVar (Maybe (Type name a))
toMap (Params _ xs) = Map.fromList (fmap (,Nothing) xs)

pop :: TyVar -> TyParamList -> (TyParamList, Maybe TyVar)
pop tyvar (Params loc xs) = case break (== tyvar) xs of
    (fs, []) -> (Params loc fs, Nothing)
    (fs, x : xs) -> (Params loc (fs <> xs), Just x)

nameOf :: UnresolvedType -> Ident
nameOf (UnresolvedType _ (TyVar name)) = name

tyVarOf :: UnresolvedType -> TyVar
tyVarOf (UnresolvedType _ tyVar) = tyVar

isTypeVar :: UnresolvedType -> TyParamList -> Bool
isTypeVar (UnresolvedType _ unresolved) (Params _loc vars) = unresolved `elem` vars

data Fn name a = Fn !(XFn a) name TyParamList [Arg name a] (Type name a) (Block name a)

type family XFn a

deriving instance (Show name, Forall Show a) => Show (Fn name a)

data Import name a
    = ImportExplicit !(XImportExplicit a) Namespace [name] -- import foo (bar, baz)
    | XImport !(XImport a)

type family XImport a

type family XImportExplicit a

deriving instance (Show name, Forall Show a) => Show (Import name a)

data Adt name a = Adt !(XAdt a) name TyParamList [Constructor name a]

type family XAdt a

deriving instance (Show name, Forall Show a) => Show (Adt name a)

data Constructor name a
    = EnumCons (XEnumCons a) name
    | FunCons (XFunCons a) name [Type name a]
    | ConstructorX !(XConstructor a)

type family XConstructor a

type family XEnumCons a

type family XFunCons a

deriving instance (Show name, Forall Show a) => Show (Constructor name a)

-- Argument
data Arg name a = Arg !(XArg a) name (Type name a)

type family XArg a

deriving instance (Show name, Forall Show a) => Show (Arg name a)

-- Type

newtype TyVar = TyVar Ident
    deriving (Data, Eq, Ord, Show)

instance Pretty TyVar where
    pretty (TyVar ident) = pretty ident

data Type name a
    = TyLit !(XTyLit a) TyLit
    | TyFun !(XTyFun a) [Type name a] (Type name a)
    | TyCon !(XTyCon a) name [Type name a]
    | TypeVar !(XTypeVar a) TyVar
    | PointerType !(XPointerType a) (Type name a)
    | Type !(XType a)

(~~) :: (Eq name, Eq (XType a)) => Type name a -> Type name a -> Bool
l ~~ r = case (l, r) of
    (TyLit _ left, TyLit _ right) -> left == right
    (TyFun _ largs lret, TyFun _ rargs rret) -> and (zipWith (~~) largs rargs) && lret ~~ rret
    (TyCon _ left typarams1, TyCon _ right typarams2) -> and (zipWith (~~) typarams1 typarams2) && left == right
    (TypeVar _ left, TypeVar _ right) -> left == right
    (Type a, Type b) -> a == b
    (PointerType _ ty1, PointerType _ ty2) -> ty1 ~~ ty2
    _ -> False

type family XTyLit a

type family XTyFun a

type family XType a

type family XTyCon a

type family XPointerType a

type family XTypeVar a

deriving instance (Forall Show a, Show name) => Show (Type name a)

coerceType ::
    ( XTyLit t1 ~ XTyLit t2
    , XTyFun t1 ~ XTyFun t2
    , XType t1 ~ XType t2
    , XTyCon t1 ~ XTyCon t2
    , XTypeVar t1 ~ XTypeVar t2
    , XPointerType t1 ~ XPointerType t2
    ) =>
    Type name t1 ->
    Type name t2
coerceType ty = case ty of
    TyLit a b -> TyLit a b
    TyFun a b c -> TyFun a (fmap coerceType b) (coerceType c)
    TyCon a b typarams -> TyCon a b (fmap coerceType typarams)
    Type a -> Type a
    TypeVar a b -> TypeVar a b
    PointerType a b -> PointerType a (coerceType b)

data TyLit = Unit | String | Int | Double | Char | Bool
    deriving (Data, Enum, Eq, Ord, Show)

data Block name a = Block !(XBlock a) [Stmt name a] (Maybe (Expr name a))

type family XBlock a

deriving instance (Show name, Forall Show a) => Show (Block name a)

-- Statement
data Stmt name a
    = SExpr !(XSExp a) (Expr name a)
    | Stmt !(XStmt a)

type family XSExp a

type family XStmt a

data AssignOp
    = AddAssign
    | SubAssign
    | MulAssign
    | DivAssign
    | ModAssign
    | Assign
    | DerefAssign
    deriving (Data, Eq, Ord, Show)

deriving instance (Show name, Forall Show a) => Show (Stmt name a)

-- Expression
data Expr name a
    = Lit !(XLit a) (Lit a)
    | Var !(XVar a) name
    | BinOp !(XBinOp a) (Expr name a) BinOp (Expr name a)
    | Prefix !(XPrefix a) PrefixOp (Expr name a)
    | App !(XApp a) (Expr name a) [Expr name a]
    | Let !(XLet a) name (Expr name a)
    | Ass !(XAss a) name AssignOp (Expr name a)
    | Ret !(XRet a) (Maybe (Expr name a))
    | EBlock !(XEBlock a) (Block name a)
    | Break !(XBreak a) (Maybe (Expr name a))
    | If !(XIf a) (Expr name a) (Block name a) (Maybe (Block name a))
    | While !(XWhile a) (Expr name a) (Block name a)
    | Loop !(XLoop a) (Block name a)
    | Lam !(XLam a) [LamArg name a] (Expr name a)
    | Match !(XMatch a) (Expr name a) [MatchArm name a]
    | Expr !(XExpr a)

deriving instance (Show name, Forall Show a) => Show (Expr name a)

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

type family XLoop a

type family XLam a

type family XMatch a

data MatchArm name a = MatchArm !(XMatchArm a) (Pattern name a) (Expr name a)

deriving instance (Show name, Forall Show a) => Show (MatchArm name a)

type family XMatchArm a

data Pattern name a
    = PVar !(XPVar a) name
    | PEnumCon !(XPEnumCon a) name
    | PFunCon !(XPFunCon a) name [Pattern name a]

deriving instance (Show name, Forall Show a) => Show (Pattern name a)

type family XPVar a

type family XPEnumCon a

type family XPFunCon a

data LamArg name a = LamArg !(XLamArg a) name

type family XLamArg a

deriving instance (Show name, Forall Show a) => Show (LamArg name a)

data PrefixOp = Not | Neg | Ref | Deref
    deriving (Data, Eq, Ord, Show)

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
    deriving (Data, Eq, Ord, Show)

-- Literal
data Lit a
    = IntLit !(XIntLit a) Integer
    | DoubleLit !(XDoubleLit a) Double
    | StringLit !(XStringLit a) Text
    | CharLit !(XCharLit a) Char
    | BoolLit !(XBoolLit a) Bool
    | UnitLit !(XUnitLit a)

type family XIntLit a

type family XDoubleLit a

type family XStringLit a

type family XCharLit a

type family XBoolLit a

type family XUnitLit a

deriving instance (Forall Show a) => Show (Lit a)

type Forall (c :: Data.Kind.Type -> Constraint) a =
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
    , c (XTyCon a)
    , c (XTyFun a)
    , c (XPointerType a)
    , c (XVar a)
    , c (XWhile a)
    , c (XEBlock a)
    , c (XExpr a)
    , c (XType a)
    , c (XLoop a)
    , c (XLam a)
    , c (XLamArg a)
    , c (XFn a)
    , c (XAdt a)
    , c (XImport a)
    , c (XImportExplicit a)
    , c (XConstructor a)
    , c (XEnumCons a)
    , c (XFunCons a)
    , c (XMatchArm a)
    , c (XMatch a)
    , c (XPVar a)
    , c (XPEnumCon a)
    , c (XPFunCon a)
    , c (XTypeVar a)
    , c (XModule a)
    )

type instance XProgram () = NoExtField

type instance XDef () = NoExtField

type instance XFn () = NoExtField

type instance XImport () = NoExtField

type instance XImportExplicit () = NoExtField

type instance XAdt () = NoExtField

type instance XConstructor () = NoExtField

type instance XEnumCons () = NoExtField

type instance XFunCons () = NoExtField

type instance XArg () = NoExtField

type instance XTyLit () = NoExtField

type instance XTyFun () = NoExtField

type instance XType () = NoExtField

type instance XTyCon () = NoExtField

type instance XTypeVar () = NoExtField

type instance XBlock () = NoExtField

type instance XSExp () = NoExtField

type instance XStmt () = NoExtField

type instance XExprStmt () = NoExtField

type instance XLit () = NoExtField

type instance XVar () = NoExtField

type instance XPrefix () = NoExtField

type instance XBinOp () = NoExtField

type instance XApp () = NoExtField

type instance XAss () = NoExtField

type instance XLet () = NoExtField

type instance XRet () = NoExtField

type instance XEBlock () = NoExtField

type instance XBreak () = NoExtField

type instance XIf () = NoExtField

type instance XWhile () = NoExtField

type instance XExpr () = NoExtField

type instance XLoop () = NoExtField

type instance XLam () = NoExtField

type instance XMatch () = NoExtField

type instance XMatchArm () = NoExtField

type instance XPVar () = NoExtField

type instance XPEnumCon () = NoExtField

type instance XPFunCon () = NoExtField

type instance XLamArg () = NoExtField

type instance XIntLit () = NoExtField

type instance XDoubleLit () = NoExtField

type instance XStringLit () = NoExtField

type instance XCharLit () = NoExtField

type instance XBoolLit () = NoExtField

type instance XUnitLit () = NoExtField
