{-# LANGUAGE LambdaCase #-}

module Backend.Desugar.Desugar where

import Backend.Desugar.Types
import Control.Lens.Getter (view)
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
    Tc.LitX (info, ty) lit -> typed ty undefined
    Tc.VarX (info, ty) name -> typed ty undefined
    Tc.BinOpX (info, ty) l op r -> typed ty undefined
    Tc.PrefixX (info, ty) op expr -> typed ty undefined
    Tc.AppX (info, ty) l rs -> typed ty undefined
    Tc.LetX info name expr -> typed (view stmtType info) undefined
    Tc.AssX info name op expr -> typed (view stmtType info) undefined
    Tc.RetX (info, ty) expr -> typed ty (Ret (maybe unit dsExpr expr))
    Tc.EBlockX NoExtField (Tc.BlockX (info, ty) stmts tail) ->
        typed ty (EBlock (Block (fmap dsStmt stmts) (fmap dsExpr tail)))
    Tc.BreakX (info, ty) mbExpr -> typed ty (Break (maybe unit dsExpr mbExpr))
    Tc.IfX (info, ty) cond trueBlk mbFalseBlk -> typed ty (iff cond trueBlk mbFalseBlk)
    Tc.WhileX (info, ty) cond block -> typed ty (while cond block)
    Tc.ExprX (LoopX (info, ty) block) -> typed ty (While true (dsBlock block))

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

true :: Expr
true = Lit $ BoolLit True

unit :: Expr
unit = Lit UnitLit

typed :: Tc.TypeTc -> Expr -> Expr
typed ty = Typed (dsType ty)
