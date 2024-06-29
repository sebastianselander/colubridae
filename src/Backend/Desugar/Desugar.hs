{-# LANGUAGE LambdaCase #-}

module Backend.Desugar.Desugar where

import Backend.Desugar.Types
import Control.Lens.Getter (view)
import Frontend.Renamer.Types qualified as Rn (Boundedness (..))
import Frontend.Typechecker.Types (stmtType)
import Frontend.Typechecker.Types qualified as Tc
import Frontend.Types (NoExtField (NoExtField), SugarStmtX (LoopX))
import Frontend.Types qualified as Tc
import Relude hiding (Type)

desugar :: Tc.ProgramTc -> Program
desugar (Tc.ProgramX _ defs) = undefined

dsDef :: Tc.DefTc -> Def
dsDef (Tc.Fn NoExtField name args ty stmts) = undefined

dsStmt :: Tc.StmtTc -> Expr
dsStmt = \case
    Tc.SExprX NoExtField expr -> dsExpr expr

dsBlock :: Tc.BlockTc -> Block
dsBlock (Tc.BlockX (info, ty) stmts tail) = undefined

dsExpr :: Tc.ExprTc -> Expr
dsExpr = \case
    Tc.LitX (info, ty) lit -> typed ty (Lit (dsLit lit))
    Tc.VarX (info, ty, binding) name -> typed ty (Var (dsBound binding) (dsIdent name))
    Tc.BinOpX (info, ty) l op r -> typed ty (BinOp (dsExpr l) (dsBinOp op) (dsExpr r))
    Tc.PrefixX (info, ty) op expr -> typed ty (PrefixOp (dsPrefixOp op) (dsExpr expr))
    Tc.AppX (info, ty) l rs -> typed ty (App (dsExpr l) (fmap dsExpr rs))
    Tc.LetX info name expr -> typed (view stmtType info) undefined
    Tc.AssX (info, binding) name op expr -> typed (view stmtType info) (ass name binding op expr)
    Tc.RetX (info, ty) expr -> typed ty (Ret (maybe unit dsExpr expr))
    Tc.EBlockX NoExtField (Tc.BlockX (info, ty) stmts tail) ->
        typed ty (EBlock (Block (fmap dsStmt stmts) (fmap dsExpr tail)))
    Tc.BreakX (info, ty) mbExpr -> typed ty (Break (maybe unit dsExpr mbExpr))
    Tc.IfX (info, ty) cond trueBlk mbFalseBlk -> typed ty (iff cond trueBlk mbFalseBlk)
    Tc.WhileX (info, ty) cond block -> typed ty (while cond block)
    Tc.ExprX (LoopX (info, ty) block) -> typed ty (While true (dsBlock block))

dsLit :: Tc.LitX Tc.Tc -> Lit
dsLit = \case
    Tc.IntLitX _ int -> IntLit int
    Tc.DoubleLitX _ double -> DoubleLit double
    Tc.StringLitX _ string -> StringLit string
    Tc.CharLitX _ char -> CharLit char
    Tc.BoolLitX _ bool -> BoolLit bool
    Tc.UnitLitX _ -> UnitLit

ass :: Tc.Ident -> Rn.Boundedness -> Tc.AssignOp -> Tc.ExprX Tc.Tc -> Expr
ass nm binding op xpr =
    let name = dsIdent nm
        expr = dsExpr xpr
        assignment operator = Ass name (BinOp (Var (dsBound binding) name) operator expr)
     in case op of
            Tc.Assign -> Ass name expr
            Tc.DivAssign -> assignment Div
            Tc.MulAssign -> assignment Mul
            Tc.AddAssign -> assignment Add
            Tc.SubAssign -> assignment Sub
            Tc.ModAssign -> assignment Mod

while :: Tc.ExprTc -> Tc.BlockTc -> Expr
while expr block = While (dsExpr expr) (dsBlock block)

iff :: Tc.ExprTc -> Tc.BlockTc -> Maybe Tc.BlockTc -> Expr
iff expr trueB falseB = If (dsExpr expr) (dsBlock trueB) (fmap dsBlock falseB)

dsType :: Tc.TypeTc -> Type
dsType = \case
    Tc.TyLitX NoExtField Tc.UnitX -> Unit
    Tc.TyLitX NoExtField Tc.StringX -> String
    Tc.TyLitX NoExtField Tc.CharX -> Char
    Tc.TyLitX NoExtField Tc.DoubleX -> Double
    Tc.TyLitX NoExtField Tc.IntX -> Int
    Tc.TyLitX NoExtField Tc.BoolX -> Bool
    Tc.TyFunX NoExtField l r -> TyFun (fmap dsType l) (dsType r)
    Tc.TypeX (Tc.MutableX ty) -> Mut (dsType ty)
    Tc.TypeX Tc.AnyX -> Unit -- NOTE: questionable

dsIdent :: Tc.Ident -> Ident
dsIdent (Tc.Ident name) = Ident name

true :: Expr
true = Lit $ BoolLit True

unit :: Expr
unit = Lit UnitLit

typed :: Tc.TypeTc -> Expr -> Expr
typed ty = Typed (dsType ty)

dsPrefixOp :: Tc.PrefixOp -> PrefixOp
dsPrefixOp = \case
    Tc.Not -> Not
    Tc.Neg -> Neg

dsBinOp :: Tc.BinOp -> BinOp
dsBinOp = \case
    Tc.Add -> Add
    Tc.Sub -> Sub
    Tc.Mul -> Mul
    Tc.Div -> Div
    Tc.Mod -> Mod
    Tc.Or -> Or
    Tc.And -> And
    Tc.Lt -> Lt
    Tc.Gt -> Gt
    Tc.Lte -> Lte
    Tc.Gte -> Gte
    Tc.Eq -> Eq
    Tc.Neq -> Neq

dsBound :: Rn.Boundedness -> Binding
dsBound = \case
    Rn.Free -> undefined
    Rn.Bound -> undefined
    Rn.Toplevel -> undefined
    Rn.Lambda -> undefined
