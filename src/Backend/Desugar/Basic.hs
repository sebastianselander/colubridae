{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Desugar.Basic (basicDesugar) where

import Backend.Desugar.Types
import Control.Lens (makeLenses)
import Control.Lens.Getter (use, view)
import Control.Lens.Setter (assign, modifying, (+=))
import Data.DList
import Frontend.Renamer.Types qualified as Rn (Boundedness (..))
import Frontend.Typechecker.Types (stmtType, varType)
import Frontend.Typechecker.Types qualified as Tc
import Frontend.Types (NoExtField (NoExtField), SugarStmtX (LoopX))
import Frontend.Types qualified as Tc
import Names (Ident (..), Names, existName, insertName)
import Origin (Origin (..))
import Relude hiding (Type, toList)

data Env = Env
    { _expressions :: DList TyExpr
    , _names :: Names
    , _nameCounter :: Int
    }

$(makeLenses ''Env)

newtype DsM a = DsM {runDsm :: StateT Env (Reader (TyExpr -> DsM ())) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadReader (TyExpr -> DsM ()))

run :: (TyExpr -> DsM ()) -> Names -> Int -> DsM a -> (a, Env)
run f names n = flip runReader f . flip runStateT (Env mempty names n) . runDsm

emit :: TyExpr -> DsM ()
emit expr = modifying expressions (`snoc` expr)

emits :: [TyExpr] -> DsM ()
emits = mapM_ emit

named :: DsM TyExpr -> DsM TyExpr
named tyExpr = do
    (Typed ty e) <- tyExpr
    name <- fresh
    emit $ Typed Unit $ Let name ty (Just (Typed ty e))
    pure (Typed ty (Var Bound name))

unitVarible :: DsM TyExpr
unitVarible = named (pure unit)

unnamed :: DsM TyExpr -> DsM ()
unnamed thing = do
    expr <- thing
    emit expr
    pure ()

declare :: Type -> DsM Ident
declare Unit = do
    name <- fresh
    emit $ Typed Unit $ Let name Unit (Just unit)
    pure name
declare ty = do
    name <- fresh
    emit $ Typed Unit $ Let name ty Nothing
    pure name

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

isMain :: Tc.DefTc -> Bool
isMain (Tc.Fn NoExtField (Ident "main") _ _ _) = True
isMain _ = False

dsDef :: Names -> Tc.DefTc -> Def
dsDef names def@(Tc.Fn NoExtField name args returnType (Tc.BlockX (_info, ty) stmts tail)) =
    let ((args', returnType'), Env emits _ _) = run (const $ pure ()) names 0 $ do
            args <- mapM dsArg args
            returnType <- dsType returnType
            case tail of
                Nothing -> mapM_ dsStmt stmts
                Just tail -> do
                    mapM_ dsStmt stmts
                    tail <- dsExpr tail
                    void $ unnamed $ typed ty (Return tail)
            pure (args, returnType)
     in if isMain def
            then Main (toList emits)
            else Fn Top name args' returnType' (toList emits)

dsArg :: Tc.ArgX Tc.Tc -> DsM Arg
dsArg (Tc.ArgX NoExtField name ty) = Arg name <$> dsType ty

dsStmt :: Tc.StmtTc -> DsM TyExpr
dsStmt = \case
    Tc.SExprX NoExtField expr -> dsExpr expr

dsExpr :: Tc.ExprTc -> DsM TyExpr
dsExpr = \case
    Tc.LitX (_info, ty) lit -> typed ty (Lit (dsLit lit))
    Tc.VarX (_info, ty, binding) name -> typed ty (Var (dsBound binding) name)
    Tc.BinOpX (_, ty) l op r -> do
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
        unnamed $ typed letTy (Let name exprTy (Just expr))
        unitVarible
    Tc.AssX (info, binding) name op expr -> do
        named $ typed (view stmtType info) =<< ass name (view varType info) binding op expr
    Tc.RetX (_info, ty) expr -> do
        expr <- mapM dsExpr expr
        unnamed $ typed ty (Return (fromMaybe unit expr))
        unitVarible
    Tc.EBlockX NoExtField block@(Tc.BlockX (_, ty) _ _) -> do
        ty <- dsType ty
        var <- declare ty
        f <- ask
        block <- dsBlock f var block
        emits block
        pure (Typed ty $ Var Bound var)
    Tc.BreakX (_info, ty) mbExpr -> do
        f <- ask
        expr <- mapM dsExpr mbExpr
        case expr of
            Nothing -> do
                unnamed $ typed ty Break
                unitVarible
            Just expr -> do
                f expr
                unnamed $ typed ty Break
                unitVarible
    Tc.IfX (_info, ty) cond trueBlk mbFalseBlk -> do
        ty' <- dsType ty
        var <- declare ty'
        cond <- dsExpr cond
        f <- ask
        trueBlk <- dsBlock f var trueBlk
        mbFalseBlk <- mapM (dsBlock f var) mbFalseBlk
        unnamed $ typed ty (If cond trueBlk mbFalseBlk)
        named $ pure $ Typed ty' (Var Bound var)
    Tc.WhileX (_info, ty) cond block -> do
        cond <- dsExpr cond
        ty' <- dsType ty
        var <- declare ty'
        block <- dsBlock (emit . Typed Unit . Ass var ty') var block
        unnamed $ typed ty (While cond block)
        named $ pure $ Typed Unit (Var Bound var)
    Tc.ExprX (LoopX (_info, ty) block) -> do
        ty' <- dsType ty
        var <- declare ty'
        block <- dsBlock (emit . Typed Unit . Ass var ty') var block
        unnamed $ typed ty (While true block)
        named $ pure $ Typed ty' (Var Bound var)

dsBlock :: (TyExpr -> DsM ()) -> Ident -> Tc.BlockX Tc.Tc -> DsM [TyExpr]
dsBlock f _ (Tc.BlockX (_, Tc.Unit) stmts tail) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n') =
            run f names' n $ mapM_ dsStmt (stmts <> maybe [] (\x -> [Tc.SExprX NoExtField x]) tail)
    assign names names''
    assign nameCounter n'
    pure (toList emits)
dsBlock f _ (Tc.BlockX _ stmts Nothing) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n') = run f names' n $ mapM_ dsStmt stmts
    assign names names''
    assign nameCounter n'
    pure (toList emits)
dsBlock f variable (Tc.BlockX (_, ty) stmts (Just tail)) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n') =
            run f names' n $ do
                ty <- dsType ty
                mapM_ dsStmt stmts
                tail <- dsExpr tail
                emit $ Typed Unit $ Ass variable ty tail
    assign names names''
    assign nameCounter n'
    pure (toList emits)

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
    Tc.TypeX Tc.AnyX -> pure Unit -- NOTE: `Any` is only the type of `return` and `break` so that they can be placed anywhere.

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
