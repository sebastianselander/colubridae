{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Desugar.Basic (basicDesugar) where

import Backend.Desugar.Types
import Control.Lens (makeLenses)
import Control.Lens.Getter (use, view)
import Control.Lens.Setter (modifying, (+=))
import Data.DList
import Frontend.Renamer.Types qualified as Rn (Boundedness (..))
import Frontend.Typechecker.Types qualified as Tc
import Frontend.Types (NoExtField (NoExtField), SugarStmtX (LoopX))
import Frontend.Types qualified as Tc
import Names (Ident (..), Names, existName, insertName)
import Relude hiding (toList, Type)
import Frontend.Typechecker.Types (stmtType, varType)

data Env = Env
    { _emits :: DList TyExpr
    , _names :: Names
    , _nameCounter :: Int
    }

$(makeLenses ''Env)

newtype DsM a = DsM {runDsm :: State Env a}
    deriving (Functor, Applicative, Monad, MonadState Env)

emit :: TyExpr -> DsM ()
emit expr = modifying emits (`snoc` expr)

named :: DsM TyExpr -> DsM TyExpr
named tyExpr = do
    (Typed ty e) <- tyExpr
    name <- fresh
    emit $ Typed Unit $ Let name ty (Typed ty e)
    pure (Typed ty (Var Bound name))

unnamed :: DsM TyExpr -> DsM TyExpr
unnamed thing = do
    expr <- thing
    emit expr
    pure expr


fresh :: DsM Ident
fresh = do
    n <- use nameCounter
    nameCounter += 1
    let freshName = Ident $ "ssa_var" <> show n
    nameMap <- use names
    if existName freshName nameMap
        then fresh
        else do
            modifying names (insertName freshName)
            pure freshName

basicDesugar :: Names -> Tc.ProgramTc -> Program
basicDesugar names (Tc.ProgramX _ defs) = Program (fmap (dsDef names) defs)

dsDef :: Names -> Tc.DefTc -> Def
dsDef names (Tc.Fn NoExtField name args returnType (Tc.BlockX (_info, ty) stmts tail)) =
    let ((args', returnType'), Env emits _ _) = flip runState (Env mempty names 0) $ runDsm $ do
            args <- mapM dsArg args
            returnType <- dsType returnType
            case tail of
                Nothing -> mapM_ dsStmt stmts
                Just tail -> do
                    mapM_ dsStmt stmts
                    tail <- dsExpr tail
                    void $ unnamed $ typed ty (Ret tail)
            pure (args, returnType)
    in Fn name args' returnType' (toList emits)


dsArg :: Tc.ArgX Tc.Tc -> DsM Arg
dsArg (Tc.ArgX NoExtField name ty) = Arg name <$> dsType ty

dsStmt :: Tc.StmtTc -> DsM TyExpr
dsStmt = \case
    Tc.SExprX NoExtField expr -> dsExpr expr

dsExpr :: Tc.ExprTc -> DsM TyExpr
dsExpr = \case
    Tc.LitX (_info, ty) lit -> named $ typed ty (Lit (dsLit lit))
    Tc.VarX (_info, ty, binding) name -> named $ typed ty (Var (dsBound binding) name)
    Tc.BinOpX (info, ty) l op r -> do
        l <- dsExpr l
        let op' = dsBinOp op
        r <- dsExpr r
        named $ typed ty (BinOp l op' r)
    Tc.PrefixX (_info, ty) op expr -> do
        let op' = dsPrefixOp op
        expr <- dsExpr expr
        named $ typed ty (PrefixOp op' expr)
    Tc.AppX (_info, ty) l rs -> do
        l <- dsExpr l
        rs <- mapM dsExpr rs
        named $ typed ty (App l rs)
    Tc.LetX info name expr -> do
        expr <- dsExpr expr
        let letTy = view stmtType info
        let exprTy = view varType info
        exprTy <- dsType exprTy
        unnamed $ typed letTy (Let name exprTy expr)
    Tc.AssX (info, binding) name op expr -> do
        named $ typed (view stmtType info) =<< ass name (view varType info) binding op expr
    Tc.RetX (_info, ty) expr -> do
        expr <- mapM dsExpr expr
        unnamed $ typed ty (Ret (fromMaybe unit expr))
    Tc.EBlockX NoExtField block -> dsBlock block
    Tc.BreakX (_info, ty) mbExpr -> do
        expr <- mapM dsExpr mbExpr
        unnamed $ typed ty $ Break (fromMaybe unit expr)
    Tc.IfX (_info, ty) cond trueBlk mbFalseBlk -> do
        cond <- dsExpr cond
        trueBlk <- dsBlock trueBlk
        mbFalseBlk <- mapM dsBlock mbFalseBlk
        named $ typed ty (If cond (error "todo") (error "todo"))

    Tc.WhileX (_info, ty) cond block -> do
        cond <- dsExpr cond
        block <- dsBlock block
        named $ typed ty (While cond (error "todo"))
    Tc.ExprX (LoopX (_info, ty) block) -> do
        block <- dsBlock block
        named $ typed ty (While true (error "todo"))

dsBlock :: Tc.BlockX Tc.Tc -> DsM TyExpr
dsBlock = undefined

dsLit :: Tc.LitX Tc.Tc -> Lit
dsLit = \case
    Tc.IntLitX _ int -> IntLit int
    Tc.DoubleLitX _ double -> DoubleLit double
    Tc.StringLitX _ string -> StringLit string
    Tc.CharLitX _ char -> CharLit char
    Tc.BoolLitX _ bool -> BoolLit bool
    Tc.UnitLitX _ -> UnitLit

ass :: Ident -> Tc.TypeTc -> Rn.Boundedness -> Tc.AssignOp -> Tc.ExprX Tc.Tc -> DsM Expr
ass name typ binding op xpr = do
    expr <- dsExpr xpr
    ty <- dsType typ
    let assignment operator =
            pure
                $ Ass
                    name
                    ty
                    (Typed ty $ BinOp (Typed ty $ Var (dsBound binding) name) operator expr)
     in case op of
            Tc.Assign -> pure $ Ass name ty expr
            Tc.DivAssign -> assignment Div
            Tc.MulAssign -> assignment Mul
            Tc.AddAssign -> assignment Add
            Tc.SubAssign -> assignment Sub
            Tc.ModAssign -> assignment Mod

dsType :: Tc.TypeTc -> DsM Type
dsType = \case
    Tc.TyLitX NoExtField Tc.UnitX -> pure Unit
    Tc.TyLitX NoExtField Tc.StringX -> pure String
    Tc.TyLitX NoExtField Tc.CharX -> pure Char
    Tc.TyLitX NoExtField Tc.DoubleX -> pure Double
    Tc.TyLitX NoExtField Tc.IntX -> pure Int
    Tc.TyLitX NoExtField Tc.BoolX -> pure Bool
    Tc.TyFunX NoExtField l r -> TyFun <$> mapM dsType l <*> dsType r
    Tc.TypeX (Tc.MutableX ty) -> Mut <$> dsType ty
    Tc.TypeX Tc.AnyX -> pure Unit -- NOTE: questionable

true :: TyExpr
true = Typed Bool $ Lit $ BoolLit True

unit :: TyExpr
unit = Typed Unit $ Lit UnitLit

typed :: Tc.TypeTc -> Expr -> DsM TyExpr
typed ty expr = Typed <$> dsType ty <*> pure expr

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
    Rn.Free -> Free
    Rn.Bound -> Bound
    Rn.Toplevel -> Toplevel
    Rn.Lambda -> Lambda
