{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Types where

import Data.Text (concat, intercalate, replicate)
import Parser.Utils
import Relude hiding (Type, concat, intercalate, replicate)
import Types

data Par

type ExprPar = ExpX Par
type ProgramPar = ProgramX Par
type LitPar = LitX Par
type ArgPar = ArgX Par
type DefPar = DefX Par
type TypePar = TypeX Par
type StmtPar = StmtX Par

deriving instance Show ProgramPar
deriving instance Show DefPar
deriving instance Show ArgPar
deriving instance Show TypePar
deriving instance Show ExprPar
deriving instance Show StmtPar
deriving instance Show LitPar

type instance XProgram Par = ()

type instance XArg Par = SourceInfo

type instance XDef Par = SourceInfo

type instance XStmt Par = ()

type instance XRet Par = ()
type instance XBlock Par = ()
type instance XBreak Par = ()
type instance XIf Par = ()
type instance XWhile Par = ()
type instance XLet Par = Mutability
type instance XSExp Par = ()

type instance XLit Par = SourceInfo
type instance XVar Par = SourceInfo
type instance XBinOp Par = SourceInfo
type instance XExprStmt Par = SourceInfo
type instance XApp Par = SourceInfo

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

class Pretty a where
  pPretty :: a -> Text

instance Pretty () where
  pPretty _ = ""

instance Pretty ProgramPar where
  pPretty = prettyProgram

instance Pretty DefPar where
  pPretty = prettyDef

instance Pretty (ArgX a) where
  pPretty = prettyArg

instance Pretty ExprPar where
  pPretty = prettyExpr1

instance Pretty (TypeX a) where
  pPretty = prettyType1

instance Pretty (LitX a) where
  pPretty = prettyLit

prettyProgram :: ProgramPar -> Text
prettyProgram (ProgramX _ defs) = intercalate "\n\n" (fmap prettyDef defs)

prettyDef :: DefPar -> Text
prettyDef (Fn _ (Ident name) args ty stmts) =
  unwords
    [ "def"
    , name
    , "(" <> intercalate ", " (fmap prettyArg args) <> ")"
    , "->"
    , prettyType1 ty
    , prettyStmt (BlockX (error "not evaluated") stmts)
    ]

prettyArg :: ArgX a -> Text
prettyArg (ArgX _ (Ident name) ty) = unwords [name, ":", prettyType1 ty]

prettyType1 :: TypeX a -> Text
prettyType1 (TyFunX _ l r) = unwords [prettyType1 l, "->", prettyType1 r]
prettyType1 ty = prettyType2 ty

prettyType2 :: TypeX a -> Text
prettyType2 = \case
  UnitX _ -> "()"
  StringX _ -> "string"
  IntX _ -> "int"
  DoubleX _ -> "double"
  CharX _ -> "char"
  BoolX _ -> "bool"
  TyVarX _ (Ident name) -> name
  ty@TyFunX {} -> concat ["(", prettyType1 ty, ")"]

prettyExpr1 :: ExprPar -> Text
prettyExpr1 (BinOpX _ l Or r) = unwords [prettyExpr1 l, "||", prettyExpr1 r]
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: ExprPar -> Text
prettyExpr2 (BinOpX _ l And r) = unwords [prettyExpr2 l, "||", prettyExpr2 r]
prettyExpr2 e = prettyExpr3 e

prettyExpr3 :: ExprPar -> Text
prettyExpr3 (BinOpX _ l Eq r) = unwords [prettyExpr3 l, "==", prettyExpr3 r]
prettyExpr3 (BinOpX _ l Neq r) = unwords [prettyExpr3 l, "!=", prettyExpr3 r]
prettyExpr3 e = prettyExpr4 e

prettyExpr4 :: ExprPar -> Text
prettyExpr4 (BinOpX _ l Lt r) = unwords [prettyExpr3 l, "<", prettyExpr3 r]
prettyExpr4 (BinOpX _ l Lte r) = unwords [prettyExpr3 l, "<=", prettyExpr3 r]
prettyExpr4 (BinOpX _ l Gt r) = unwords [prettyExpr3 l, ">", prettyExpr3 r]
prettyExpr4 (BinOpX _ l Gte r) = unwords [prettyExpr3 l, ">=", prettyExpr3 r]
prettyExpr4 e = prettyExpr5 e

prettyExpr5 :: ExprPar -> Text
prettyExpr5 (BinOpX _ l Add r) = unwords [prettyExpr5 l, "+", prettyExpr5 r]
prettyExpr5 (BinOpX _ l Sub r) = unwords [prettyExpr5 l, "-", prettyExpr5 r]
prettyExpr5 e = prettyExpr6 e

prettyExpr6 :: ExprPar -> Text
prettyExpr6 (BinOpX _ l Mod r) = unwords [prettyExpr6 l, "%", prettyExpr6 r]
prettyExpr6 (BinOpX _ l Div r) = unwords [prettyExpr6 l, "/", prettyExpr6 r]
prettyExpr6 e@BinOpX {} = concat ["(", prettyExpr1 e, ")"]
prettyExpr6 (LitX _ lit) = prettyLit lit
prettyExpr6 (VarX _ (Ident name)) = name
prettyExpr6 (EStmtX _ s) = prettyStmt s
prettyExpr6 (AppX _ l rs) = concat [prettyExpr1 l, "(", intercalate ", " $ fmap prettyExpr1 rs,")"]

prettyStmt :: StmtPar -> Text
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
  let mut = case m of
        Mutable -> "let mut"
        Immutable -> "let"
   in unwords [mut, name, "=", prettyExpr1 e]
prettyStmt (SExpX _ e) = prettyExpr1 e
prettyStmt (StmtX a) = pPretty a

prettyLit :: LitX a -> Text
prettyLit l = case l of
  IntLitX _ l -> show l
  DoubleLitX _ l -> show l
  StringLitX _ l -> show l
  CharLitX _ l -> show l
  BoolLitX _ l -> show l

indent :: Int -> Text -> Text
indent n t = replicate n " " <> t
