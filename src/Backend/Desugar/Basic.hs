{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Desugar.Basic (basicDesugar) where

import Backend.Desugar.Types
import Backend.Llvm.Prelude (globalUnit)
import Control.Lens (makeLenses)
import Control.Lens.Getter (use, view)
import Control.Lens.Setter (assign, modifying)
import Data.DList
import Data.List (nub)
import Data.Text qualified as Text
import Frontend.Renamer.Types qualified as Rn (Boundedness (..))
import Frontend.Typechecker.Types (stmtType, varType)
import Frontend.Typechecker.Types qualified as Tc
import Frontend.Types (NoExtField (NoExtField))
import Frontend.Types qualified as Tc
import Names (Ident (..), Names, existName, insertName)
import Origin (Origin (..))
import Relude hiding (Type, fromList, toList)
import Utils (listify')

data Env = Env
    { _expressions :: DList TyExpr
    , _names :: Names
    , _nameCounter :: Int
    , _liftedLambdas :: DList Def
    }

$(makeLenses ''Env)

newtype DsM a = DsM {runDsm :: StateT Env (Reader (TyExpr -> DsM ())) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadReader (TyExpr -> DsM ()))

run :: (TyExpr -> DsM ()) -> Names -> Int -> DsM a -> (a, Env)
run f names n = flip runReader f . flip runStateT (Env mempty names n mempty) . runDsm

emit :: TyExpr -> DsM ()
emit expr = modifying expressions (`snoc` expr)

emits :: [TyExpr] -> DsM ()
emits = mapM_ emit

named :: DsM TyExpr -> DsM TyExpr
named tyExpr = do
    (Typed ty e) <- tyExpr
    name <- fresh "named"
    emit $ Typed Unit $ Let name ty (Just (Typed ty e))
    pure (Typed ty (Var Bound name))

unitVarible :: DsM TyExpr
unitVarible = named (pure unit)

unitGlobalName :: Ident
unitGlobalName = Ident (Text.pack globalUnit)

unitGlobalVariable :: DsM TyExpr
unitGlobalVariable = pure $ Typed Unit (Var GlblConst unitGlobalName)

unnamed :: DsM TyExpr -> DsM ()
unnamed thing = do
    expr <- thing
    emit expr
    pure ()

declare :: Type -> DsM Ident
declare Unit = do
    name <- fresh "declare"
    emit $ Typed Unit $ Let name Unit (Just unit)
    pure name
declare ty = do
    name <- fresh "declare"
    emit $ Typed Unit $ Let name ty Nothing
    pure name

fresh :: Text -> DsM Ident
fresh prefix = go 0
  where
    go :: Int -> DsM Ident
    go n = do
        let freshName = Ident $ prefix <> "_" <> show n
        nameMap <- use names
        if existName freshName nameMap
            then go (n + 1)
            else do
                modifying names (insertName freshName)
                pure freshName

basicDesugar :: Names -> Tc.ProgramTc -> Program
basicDesugar names = fst . run (const $ pure ()) names 0 . dsProgram

dsProgram :: Tc.ProgramTc -> DsM Program
dsProgram (Tc.ProgramX Tc.NoExtField defs) = do
    defs <- mapM dsDef defs
    lifteds <- use liftedLambdas
    pure $ Program $ toList lifteds <> defs

isMain :: Tc.DefTc -> Bool
isMain (Tc.Fn NoExtField (Ident "main") _ _ _) = True
isMain _ = False

dsDef :: Tc.DefTc -> DsM Def
dsDef def@(Tc.Fn NoExtField name args returnType (Tc.BlockX (_info, ty) stmts tail)) = do
    assign nameCounter 0 -- Start the name counter from 0 for each local scope
    args <- (Arg env VoidPtr :) <$> mapM dsArg args
    returnType <- dsType returnType
    case tail of
        Nothing -> mapM_ dsStmt stmts
        Just tail -> do
            mapM_ dsStmt stmts
            tail <- dsExpr tail
            if isMain def
                then void (unnamed (pure tail))
                else void $ unnamed $ typed ty (Return tail)
    emits <- use expressions
    assign expressions mempty
    if isMain def
        then pure $ Main (toList emits)
        else case returnType of
            Unit -> pure $ Fn Top name args returnType (toList $ emits `snoc` Typed Unit (Return unit))
            _ -> pure $ Fn Top name args returnType (toList emits)

dsArg :: Tc.ArgX Tc.Tc -> DsM Arg
dsArg (Tc.ArgX NoExtField name ty) = Arg name <$> dsType ty

dsStmt :: Tc.StmtTc -> DsM TyExpr
dsStmt = \case
    Tc.SExprX NoExtField expr -> dsExpr expr

dsExpr :: Tc.ExprTc -> DsM TyExpr
dsExpr = \case
    Tc.LitX (_info, ty) lit -> named $ typed ty (Lit (dsLit lit))
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
        case l of
            Typed _ (Var Toplevel _) -> do
                named $ typed ty (App l (Typed VoidPtr (Lit NullLit) : rs))
            Typed lty l -> do
                let tupleTy = Tuple [lty, VoidPtr]
                let function = StructIndexing (Typed tupleTy l) 0
                let env = StructIndexing (Typed tupleTy l) 1
                named $ typed ty (App (Typed tupleTy function) (Typed VoidPtr env : rs))
    Tc.LetX info name expr -> do
        (list, expr) <- contextually $ dsExpr expr
        case expr of
            Typed _ (Var Toplevel _) -> do
                varty <- dsType $ view varType info
                let tupleTy = Tuple [varty, VoidPtr]
                unnamed
                    $ typed
                        (view stmtType info)
                        (Let name tupleTy (Just $ Typed tupleTy $ Closure expr []))
                unitGlobalVariable
            _ -> do
                mapM_ emit list
                let letTy = view stmtType info
                let exprTy = view varType info
                exprTy <- dsType exprTy
                unnamed $ typed letTy (Let name exprTy (Just expr))
                unitGlobalVariable
    Tc.AssX (info, binding) name op expr -> do
        named $ typed (view stmtType info) =<< ass name (view varType info) binding op expr
    Tc.RetX (_info, ty) expr -> do
        expr <- mapM dsExpr expr
        unnamed $ typed ty (Return (fromMaybe unit expr))
        unitGlobalVariable
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
                unitGlobalVariable
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
    Tc.LoopX (_info, ty) block -> do
        ty' <- dsType ty
        var <- declare ty'
        block <- dsBlock (emit . Typed Unit . Ass var ty') var block
        unnamed $ typed ty (While true block)
        named $ pure $ Typed ty' (Var Bound var)
    Tc.LamX (_info, ty) lamArgs body -> do
        args <- mapM mkArg lamArgs
        freshName <- fresh "lambda"
        ty' <- dsType ty
        returnType <- case ty of
            Tc.TyFunX Tc.NoExtField _ retty -> dsType retty
            nonFunTy ->
                error
                    $ "Internal compiler bug: non-function type '"
                    <> show nonFunTy
                    <> "' on lambda when lifting"
        (lambdaBody, expr) <- contextually $ dsExpr body
        let freeVariables = sort $ nub $ concatMap freeVars (expr : toList lambdaBody)
        let declareFrees = zipWith (lookupFree env) [0 ..] freeVariables
        let liftedLambda =
                Fn
                    Lifted
                    freshName
                    (Arg env VoidPtr : args)
                    returnType
                    (declareFrees <> toList (lambdaBody `snoc` Typed returnType (Return expr)))
        modifying liftedLambdas (`snoc` liftedLambda)
        pure
            $ Typed
                ty'
                ( Closure
                    (Typed ty' (Var Toplevel freshName))
                    (fmap (\(ty, name) -> Typed ty (Var Free name)) freeVariables)
                )

env :: Ident
env = Ident "env"

lookupFree :: Ident -> Integer -> (Type, Ident) -> TyExpr
lookupFree env n (ty, var) =
    Typed Unit
        $ Let var ty (Just (Typed ty $ PtrIndexing (Typed VoidPtr (Var Argument env)) n))

freeVars :: TyExpr -> [(Type, Ident)]
freeVars = listify' free
  where
    free (Typed ty (Var Free name)) = Just (ty, name)
    free _ = Nothing

mkArg :: (Monad m) => Tc.LamArgTc -> m Arg
mkArg (Tc.LamArgX ty name) = do
    ty <- dsType ty
    pure (Arg name ty)

dsBlock :: (TyExpr -> DsM ()) -> Ident -> Tc.BlockX Tc.Tc -> DsM [TyExpr]
dsBlock f _ (Tc.BlockX (_, Tc.Unit) stmts tail) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds) =
            run f names' n $ mapM_ dsStmt (stmts <> maybe [] (\x -> [Tc.SExprX NoExtField x]) tail)
    assign names names''
    assign nameCounter n'
    assign liftedLambdas lifteds
    pure (toList emits)
dsBlock f _ (Tc.BlockX _ stmts Nothing) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds) = run f names' n $ mapM_ dsStmt stmts
    assign names names''
    assign nameCounter n'
    assign liftedLambdas lifteds
    pure (toList emits)
dsBlock f variable (Tc.BlockX (_, ty) stmts (Just tail)) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds) =
            run f names' n $ do
                ty <- dsType ty
                mapM_ dsStmt stmts
                tail <- dsExpr tail
                emit $ Typed Unit $ Ass variable ty tail
    assign names names''
    assign nameCounter n'
    assign liftedLambdas lifteds
    pure (toList emits)

dsLit :: Tc.LitX Tc.Tc -> Lit
dsLit = \case
    Tc.IntLitX _ int -> IntLit int
    Tc.DoubleLitX _ double -> DoubleLit double
    Tc.StringLitX _ _ -> error "TODO: String literal"
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

dsType :: (Monad m) => Tc.TypeTc -> m Type
dsType = \case
    Tc.TyLitX NoExtField Tc.UnitX -> pure Unit
    Tc.TyLitX NoExtField Tc.StringX -> pure String
    Tc.TyLitX NoExtField Tc.CharX -> pure Char
    Tc.TyLitX NoExtField Tc.DoubleX -> pure Double
    Tc.TyLitX NoExtField Tc.IntX -> pure Int
    Tc.TyLitX NoExtField Tc.BoolX -> pure Bool
    Tc.TyFunX NoExtField l r -> do
        ls <- mapM dsType l
        r <- dsType r
        pure $ TyFun (VoidPtr : ls) r
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
    Rn.Argument -> Argument

contextually :: DsM a -> DsM (DList TyExpr, a)
contextually m = do
    nc <- use nameCounter
    exprs <- use expressions
    assign expressions mempty
    e <- m
    emits <- use expressions
    assign nameCounter nc
    assign expressions exprs 
    pure (emits, e)
