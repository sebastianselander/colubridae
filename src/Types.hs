{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Types where

import Data.Data (Data)
import Relude hiding (Type, concat, intercalate, replicate)
import Relude qualified
import qualified Data.Kind
import Text.Megaparsec (Pos)
import GHC.Show (show)
import Data.Tuple.Extra (both)
import Text.Megaparsec.Pos (unPos)
import qualified Data.Text as Text

data Mutability = Mutable | Immutable
  deriving (Show, Eq, Ord, Data, Typeable)

data Span = Span
  { start :: !(Pos, Pos)
  , end :: !(Pos, Pos)
  }
  deriving (Eq, Ord, Data)

instance Show Span where
  show Span {start, end} = let (bl, bc) = both (Relude.show . unPos) start 
                               (al, ac) = both (Relude.show . unPos) end
                            in Relude.concat ["(",bl, ":", bc, "->", al, ":", ac,")"]

data SourceInfo = SourceInfo
  { spanInfo :: !(Maybe Span)
  , sourceFile :: !FilePath
  }
  deriving (Eq, Ord, Data)

instance Show SourceInfo where
  show info = maybe "no pos" Relude.show info.spanInfo


-- Program
data ProgramX a = ProgramX (XProgram a) [DefX a]
type family XProgram a

deriving instance ForallX Show a => Show (ProgramX a)
deriving instance ForallX Typeable a => Typeable (ProgramX a)

-- Definition
data DefX a = Fn (XDef a) Ident [ArgX a] (TypeX a) [StmtX a]
type family XDef a

deriving instance ForallX Show a => Show (DefX a)
deriving instance ForallX Typeable a => Typeable (DefX a)

-- Argument
data ArgX a = ArgX (XArg a) Ident (TypeX a)
type family XArg a

deriving instance ForallX Show a => Show (ArgX a)
deriving instance ForallX Typeable a => Typeable (ArgX a)

-- Type
data TypeX a
  = UnitX (XUnit a)
  | StringX (XString a)
  | IntX (XInt a)
  | DoubleX (XDouble a)
  | CharX (XChar a)
  | BoolX (XBool a)
  | TyVarX (XTyVar a) Ident
  | TyFunX (XTyFun a) (TypeX a) (TypeX a)
  | TypeX (XType a)
type family XUnit a
type family XString a
type family XInt a
type family XDouble a
type family XChar a
type family XBool a
type family XTyVar a
type family XTyFun a
type family XType a

deriving instance ForallX Show a => Show (TypeX a)
deriving instance ForallX Typeable a => Typeable (TypeX a)

-- Statement
data StmtX a
  = RetX (XRet a) (Maybe (ExprX a))
  | BlockX (XBlock a) [StmtX a]
  | BreakX (XBreak a) (Maybe (ExprX a))
  | IfX (XIf a) (ExprX a) [StmtX a] (Maybe [StmtX a])
  | WhileX (XWhile a) (ExprX a) [StmtX a]
  | LetX (XLet a) Ident (ExprX a)
  | AssX (XAss a) Ident (ExprX a)
  | SExprX (XSExp a) (ExprX a)
  | StmtX (XStmt a)
type family XRet a
type family XBlock a
type family XBreak a
type family XIf a
type family XWhile a
type family XLet a
type family XAss a
type family XSExp a
type family XStmt a

deriving instance ForallX Show a => Show (StmtX a)
deriving instance ForallX Typeable a => Typeable (StmtX a)

-- Expression
data ExprX a
  = LitX (XLit a) (LitX a)
  | VarX (XVar a) Ident
  | BinOpX (XBinOp a) (ExprX a) BinOp (ExprX a)
  | AppX (XApp a) (ExprX a) [ExprX a]
  | EStmtX (XExprStmt a) (StmtX a)
  | ExprX (XExpr a)
type family XExprStmt a
type family XLit a
type family XVar a
type family XBinOp a
type family XApp a
type family XExpr a

deriving instance ForallX Show a => Show (ExprX a)
deriving instance ForallX Typeable a => Typeable (ExprX a)

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
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | Assign
  deriving (Show, Eq, Ord, Data)

-- Literal
data LitX a
  = IntLitX (XIntLit a) Integer
  | DoubleLitX (XDoubleLit a) Double
  | StringLitX (XStringLit a) Text
  | CharLitX (XCharLit a) Char
  | BoolLitX (XBoolLit a) Bool
type family XIntLit a
type family XDoubleLit a
type family XStringLit a
type family XCharLit a
type family XBoolLit a

deriving instance ForallX Show a => Show (LitX a)
deriving instance ForallX Typeable a => Typeable (LitX a)

-- Identifier
newtype Ident = Ident Text
  deriving (Show, Eq, Ord, Data, Semigroup, Monoid)

type ForallX (c :: Data.Kind.Type -> Constraint) a =
  ( c (XApp a)
  , c (XArg a)
  , c (XBinOp a)
  , c (XBlock a)
  , c (XBool a)
  , c (XBoolLit a)
  , c (XBreak a)
  , c (XChar a)
  , c (XCharLit a)
  , c (XDef a)
  , c (XDouble a)
  , c (XDoubleLit a)
  , c (XExprStmt a)
  , c (XIf a)
  , c (XInt a)
  , c (XIntLit a)
  , c (XLet a)
  , c (XAss a)
  , c (XLit a)
  , c (XProgram a)
  , c (XRet a)
  , c (XSExp a)
  , c (XStmt a)
  , c (XString a)
  , c (XStringLit a)
  , c (XTyFun a)
  , c (XTyVar a)
  , c (XUnit a)
  , c (XVar a)
  , c (XWhile a)
  , c (XExpr a)
  , c (XType a)
  )

class Pretty a where
  pPretty :: a -> Text

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b) => Pretty (a,b) where
  pPretty (a,b) = case (pPretty a, pPretty b) of
      ("", b) -> b
      (a, "") -> a
      (a, b) -> "(" <> a <> ", " <> b <> ")"

instance Pretty Mutability where
    pPretty Mutable = "mut"
    pPretty Immutable = ""

instance  Pretty (SourceInfo, Mutability) where
   pPretty (_, mut) = pPretty mut

instance Pretty SourceInfo where
    pPretty = Relude.show

instance Pretty Void where
    pPretty _ = ""

instance Pretty () where
  pPretty _ = ""

instance Pretty a => Pretty (Maybe a) where
  pPretty Nothing = ""
  pPretty (Just a) = pPretty a
    
instance ForallX Pretty a => Pretty (ProgramX a) where
  pPretty = prettyProgram

instance ForallX Pretty a => Pretty (DefX a) where
  pPretty = prettyDef

instance ForallX Pretty a => Pretty (ArgX a) where
  pPretty = prettyArg

instance ForallX Pretty a => Pretty (ExprX a) where
  pPretty = prettyExpr1

instance ForallX Pretty a => Pretty (TypeX a) where
  pPretty = prettyType1

instance ForallX Pretty a => Pretty (LitX a) where
  pPretty = prettyLit

prettyProgram :: ForallX Pretty a => ProgramX a -> Text
prettyProgram (ProgramX _ defs) = Text.intercalate "\n\n" (fmap prettyDef defs)

prettyDef :: ForallX Pretty a => DefX a -> Text
prettyDef (Fn _ (Ident name) args ty stmts) =
  unwords
    [ "def"
    , name
    , "(" <> Text.intercalate ", " (fmap prettyArg args) <> ")"
    , "->"
    , prettyType1 ty
    , prettyStmt (BlockX (error "not evaluated") stmts)
    ]

prettyArg :: ForallX Pretty a => ArgX a -> Text
prettyArg (ArgX a (Ident name) ty) = unwords [pPretty a, name, ":", prettyType1 ty]

prettyType1 :: ForallX Pretty a => TypeX a -> Text
prettyType1 (TyFunX _ l r) = unwords [prettyType1 l, "->", prettyType1 r]
prettyType1 ty = prettyType2 ty

prettyType2 :: ForallX Pretty a => TypeX a -> Text
prettyType2 = \case
  UnitX _ -> "()"
  StringX _ -> "string"
  IntX _ -> "int"
  DoubleX _ -> "double"
  CharX _ -> "char"
  BoolX _ -> "bool"
  TyVarX _ (Ident name) -> name
  ty@TyFunX {} -> Text.concat ["(", prettyType1 ty, ")"]
  TypeX a -> pPretty a

prettyExpr1 :: ForallX Pretty a => ExprX a -> Text
prettyExpr1 (BinOpX _ l Or r) = unwords [prettyExpr2 l, "||", prettyExpr1 r]
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: ForallX Pretty a => ExprX a -> Text
prettyExpr2 (BinOpX _ l And r) = unwords [prettyExpr3 l, "||", prettyExpr2 r]
prettyExpr2 e = prettyExpr3 e

prettyExpr3 :: ForallX Pretty a => ExprX a -> Text
prettyExpr3 (BinOpX _ l Eq r) = unwords [prettyExpr4 l, "==", prettyExpr3 r]
prettyExpr3 (BinOpX _ l Neq r) = unwords [prettyExpr4 l, "!=", prettyExpr3 r]
prettyExpr3 e = prettyExpr4 e

prettyExpr4 :: ForallX Pretty a => ExprX a -> Text
prettyExpr4 (BinOpX _ l Lt r) = unwords [prettyExpr4 l, "<", prettyExpr3 r]
prettyExpr4 (BinOpX _ l Lte r) = unwords [prettyExpr4 l, "<=", prettyExpr3 r]
prettyExpr4 (BinOpX _ l Gt r) = unwords [prettyExpr4 l, ">", prettyExpr3 r]
prettyExpr4 (BinOpX _ l Gte r) = unwords [prettyExpr4 l, ">=", prettyExpr3 r]
prettyExpr4 e = prettyExpr5 e

prettyExpr5 :: ForallX Pretty a => ExprX a -> Text
prettyExpr5 (BinOpX _ l Add r) = unwords [prettyExpr6 l, "+", prettyExpr5 r]
prettyExpr5 (BinOpX _ l Sub r) = unwords [prettyExpr6 l, "-", prettyExpr5 r]
prettyExpr5 e = prettyExpr6 e

prettyExpr6 :: ForallX Pretty a => ExprX a -> Text
prettyExpr6 (BinOpX _ l Mod r) = unwords [prettyExpr7 l, "%", prettyExpr6 r]
prettyExpr6 (BinOpX _ l Div r) = unwords [prettyExpr7 l, "/", prettyExpr6 r]
prettyExpr6 (BinOpX _ l Mul r) = unwords [prettyExpr7 l, "*", prettyExpr6 r]
prettyExpr6 e = prettyExpr7 e

prettyExpr7 :: ForallX Pretty a => ExprX a -> Text
prettyExpr7 e@BinOpX {} = Text.concat ["(", prettyExpr1 e, ")"]
prettyExpr7 (LitX _ lit) = prettyLit lit
prettyExpr7 (VarX _ (Ident name)) = name
prettyExpr7 (EStmtX _ s) = prettyStmt s
prettyExpr7 (AppX _ l rs) = Text.concat [prettyExpr1 l, "(", Text.intercalate ", " $ fmap prettyExpr1 rs, ")"]
prettyExpr7 (ExprX a) = pPretty a

prettyStmt :: ForallX Pretty a => StmtX a -> Text
prettyStmt (RetX _ Nothing) = "return"
prettyStmt (RetX _ (Just e)) = unwords ["return", prettyExpr1 e]
prettyStmt (BlockX _ stmts) =
  "{\n" <> unlines (fmap (indent 4 . prettyStmt) stmts) <> "}"
prettyStmt (BreakX _ Nothing) = "break"
prettyStmt (BreakX _ (Just e)) = unwords ["break", prettyExpr1 e]
prettyStmt (IfX _ cond thenB Nothing) =
  unwords
    [ "if"
    , prettyExpr1 cond
    , prettyStmt (BlockX (error "not evaluated") thenB)
    ]
prettyStmt (IfX _ cond thenB (Just elseB)) =
  unwords
    [ "if"
    , prettyExpr1 cond
    , prettyStmt $ BlockX (error "not evaluated") thenB
    , "else"
    , prettyStmt $ BlockX (error "not evaluated") elseB
    ]
prettyStmt (WhileX _ cond block) =
  unwords
    [ "while"
    , prettyExpr1 cond
    , prettyStmt $ BlockX (error "not evaluated") block
    ]
prettyStmt (LetX m (Ident name) e) =
  let mut = "let " <> pPretty m
   in unwords [mut, name, "=", prettyExpr1 e]
prettyStmt (AssX _ (Ident name) e) = unwords [name, "=", prettyExpr1 e]
prettyStmt (SExprX _ e) = prettyExpr1 e
prettyStmt (StmtX a) = pPretty a

prettyLit :: ForallX Pretty a => LitX a -> Text
prettyLit l = case l of
  IntLitX _ l -> Relude.show l
  DoubleLitX _ l -> Relude.show l
  StringLitX _ l -> Relude.show l
  CharLitX _ l -> Relude.show l
  BoolLitX _ l -> Relude.show l

indent :: Int -> Text -> Text
indent n t = Text.replicate n " " <> t
