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
import Data.List (nub, (\\))
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
import Data.Tuple.Extra (uncurry3)

data Env = Env
    { _expressions :: DList TyExpr
    , _names :: Names
    , _nameCounter :: Int
    , _lifted :: DList Def
    , _staticStrings :: [(Ident, Type, Text)]
    }

$(makeLenses ''Env)

newtype DsM a = DsM {runDsm :: StateT Env (Reader (TyExpr -> DsM ())) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadReader (TyExpr -> DsM ()))

run :: (TyExpr -> DsM ()) -> Names -> Int -> DsM a -> (a, Env)
run f names n = flip runReader f . flip runStateT (Env mempty names n mempty mempty) . runDsm

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
    lifteds <- use lifted
    strings <- use staticStrings
    pure $ Program $ fmap (uncurry3 StaticString) strings <> toList lifteds <> defs

isMain :: Tc.FnTc -> Bool
isMain (Tc.Fn NoExtField (Ident "main") _ _ _) = True
isMain _ = False

dsFunction :: Tc.FnTc -> DsM Def
dsFunction def@(Tc.Fn NoExtField name args returnType (Tc.BlockX (_info, _) stmts tail)) = do
    assign nameCounter 0 -- Start the name counter from 0 for each local scope
    args <- (EnvArg (Ptr Void) :) <$> mapM dsArg args
    returnType <- mkClosureType returnType
    case tail of
        Nothing -> mapM_ dsStmt stmts
        Just tail -> do
            mapM_ dsStmt stmts
            tail <- dsExpr tail
            if isMain def
                then void (unnamed (pure tail))
                else void $ unnamed $ pure $ Typed returnType (Return tail)
    emits <- use expressions
    assign expressions mempty
    if isMain def
        then pure $ Main (toList emits)
        else case returnType of
            Unit -> pure $ Fn Top name args returnType (toList $ emits `snoc` Typed Unit (Return unit))
            _ -> pure $ Fn Top name args returnType (toList emits)

dsDef :: Tc.DefTc -> DsM Def
dsDef (Tc.DefFn fn) = dsFunction fn
dsDef (Tc.DefAdt adt) = undefined

dsArg :: Tc.ArgX Tc.Tc -> DsM Arg
dsArg (Tc.ArgX NoExtField name ty) = Arg name <$> mkClosureType ty

dsStmt :: Tc.StmtTc -> DsM TyExpr
dsStmt = \case
    Tc.SExprX NoExtField expr -> dsExpr expr

dsExpr :: Tc.ExprTc -> DsM TyExpr
dsExpr = \case
    Tc.LitX (_info, ty) lit -> do
        lit <- dsLit lit
        named $ typed ty lit
    Tc.VarX (_info, ty, binding) name -> do
        ty <- mkClosureType ty
        pure $ Typed ty (Var (dsBound binding) name)
    Tc.BinOpX (_, ty) l op r -> do
        l <- dsExpr l
        let op' = dsBinOp op
        r <- dsExpr r
        typed ty (BinOp l op' r)
    Tc.PrefixX (_info, ty) op expr -> do
        let op' = dsPrefixOp op
        expr <- dsExpr expr
        typed ty (PrefixOp op' expr)
    Tc.AppX (_info, ty) l rs -> do
        l <- dsExpr l
        rs <- mapM dsExpr rs
        ty <- mkClosureType ty
        case l of
            Typed _ (Var Toplevel _) -> named $ pure $ Typed ty (App l (Typed (Ptr Void) (Lit NullLit) : rs))
            Typed lty l -> do
                let function = StructIndexing (Typed lty l) 0
                let env = StructIndexing (Typed lty l) 1
                named $ pure $ Typed ty (App (Typed lty function) (Typed (Ptr Void) env : rs))
    Tc.LetX info name expr -> do
        (list, expr) <- contextually $ dsExpr expr
        case expr of
            Typed _ (Var Toplevel _) -> do
                varty <- dsType $ view varType info
                let tupleTy = Struct [varty, Ptr Void]
                unnamed
                    $ typed
                        (view stmtType info)
                        (Let name tupleTy (Just $ Typed tupleTy $ Closure expr []))
                unitGlobalVariable
            _ -> do
                mapM_ emit list
                let letTy = view stmtType info
                let exprTy = view varType info
                exprTy <- mkClosureType exprTy
                unnamed $ typed letTy (Let name exprTy (Just expr))
                unitGlobalVariable
    Tc.AssX (info, binding) name op expr -> do
        named $ typed (view stmtType info) =<< ass name (view varType info) binding op expr
    Tc.RetX (_info, ty) expr -> do
        expr <- mapM dsExpr expr
        unnamed $ typed ty (Return (fromMaybe unit expr))
        unitGlobalVariable
    Tc.EBlockX NoExtField block@(Tc.BlockX (_, ty) _ _) -> do
        ty <- mkClosureType ty
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
        ty' <- mkClosureType ty
        var <- declare ty'
        cond <- dsExpr cond
        f <- ask
        trueBlk <- dsBlock f var trueBlk
        mbFalseBlk <- mapM (dsBlock f var) mbFalseBlk
        unnamed $ typed ty (If cond trueBlk mbFalseBlk)
        named $ pure $ Typed ty' (Var Bound var)
    Tc.WhileX (_info, ty) cond block -> do
        cond <- dsExpr cond
        ty' <- mkClosureType ty
        var <- declare ty'
        block <- dsBlock (emit . Typed Unit . Ass var ty') var block
        unnamed $ typed ty (While cond block)
        named $ pure $ Typed Unit (Var Bound var)
    Tc.LoopX (_info, ty) block -> do
        ty' <- mkClosureType ty
        var <- declare ty'
        block <- dsBlock (emit . Typed Unit . Ass var ty') var block
        unnamed $ typed ty (While true block)
        named $ pure $ Typed ty' (Var Bound var)
    Tc.LamX (_info, ty) lamArgs body -> do
        args <- mapM mkArg lamArgs
        freshName <- fresh "lambda"
        ty' <- dsType ty
        returnType <- case ty of
            Tc.TyFunX Tc.NoExtField _ retty -> mkClosureType retty
            nonFunTy ->
                error
                    $ "Internal compiler bug: non-function type '"
                    <> show nonFunTy
                    <> "' on lambda when lifting"
        (lambdaBody, expr) <- contextually $ dsExpr body
        let freeVariables = sort $ nub $ concatMap (freeVars (boundArgs args)) (expr : toList lambdaBody)
        let declareFrees = zipWith (lookupFree env) [0 ..] freeVariables
        closureTy <- mkClosureType ty
        modifying
            lifted
            ( `snoc`
                Fn
                    Lifted
                    freshName
                    (EnvArg (Ptr Void) : args)
                    returnType
                    (declareFrees <> toList (lambdaBody `snoc` Typed returnType (Return expr)))
            )
        pure
            $ Typed
                closureTy
                ( Closure
                    (Typed ty' (Var Toplevel freshName))
                    (fmap (\(ty, name) -> Typed ty (Var Free name)) freeVariables)
                )

boundArgs :: [Arg] -> [(Type, Ident)]
boundArgs [] = []
boundArgs (x:xs) = case x of
    Arg name ty -> (ty,name) : boundArgs xs
    _ -> boundArgs xs

mkClosureType :: Monad m => Tc.TypeTc -> m Type
mkClosureType (Tc.TyFunX NoExtField ls r) = do
    ls <- mapM mkClosureType ls
    r <- mkClosureType r
    pure $ Struct [TyFun ls r, Ptr Void]
mkClosureType (Tc.TypeX (Tc.MutableX ty)) = Mut <$> mkClosureType ty
mkClosureType ty = dsType ty

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
        pure $ TyFun (Ptr Void : ls) r
    Tc.TypeX (Tc.MutableX ty) -> Mut <$> dsType ty
    Tc.TypeX Tc.AnyX -> pure Unit -- NOTE: `Any` is only the type of `return` and `break` so that they can be placed anywhere.


env :: Ident
env = Ident "env"

lookupFree :: Ident -> Integer -> (Type, Ident) -> TyExpr
lookupFree env n (ty, var) = Typed ty $ ExtractFree var env n

freeVars :: [(Type, Ident)] -> TyExpr -> [(Type, Ident)]
freeVars xs expr = listify' free expr \\ xs
  where
    free (Typed ty (Var Free name)) = Just (ty, name)
    free (Typed ty (Var Argument name)) = Just (ty, name)
    free _ = Nothing

mkArg :: (Monad m) => Tc.LamArgTc -> m Arg
mkArg (Tc.LamArgX ty name) = do
    ty <- mkClosureType ty
    pure (Arg name ty)

dsBlock :: (TyExpr -> DsM ()) -> Ident -> Tc.BlockX Tc.Tc -> DsM [TyExpr]
dsBlock f _ (Tc.BlockX (_, Tc.Unit) stmts tail) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds strings) =
            run f names' n $ mapM_ dsStmt (stmts <> maybe [] (\x -> [Tc.SExprX NoExtField x]) tail)
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)
dsBlock f _ (Tc.BlockX _ stmts Nothing) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds strings) = run f names' n $ mapM_ dsStmt stmts
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)
dsBlock f variable (Tc.BlockX (_, ty) stmts (Just tail)) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds strings) =
            run f names' n $ do
                ty <- mkClosureType ty
                mapM_ dsStmt stmts
                tail <- dsExpr tail
                emit $ Typed Unit $ Ass variable ty tail
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)

dsLit :: Tc.LitTc -> DsM Expr
dsLit = \case
    Tc.IntLitX NoExtField int -> pure $ Lit $ IntLit int
    Tc.DoubleLitX NoExtField double -> pure $ Lit $ DoubleLit double
    Tc.StringLitX NoExtField string -> do
        name <- fresh "static_string"
        modifying staticStrings ((name, Array (Text.length string + 1) Char, string <> "\\00") :)
        pure (Var Toplevel name)
    Tc.CharLitX NoExtField char -> pure $ Lit $ CharLit char
    Tc.BoolLitX NoExtField bool -> pure $ Lit $ BoolLit bool
    Tc.UnitLitX NoExtField -> pure $ Lit UnitLit

ass :: Ident -> Tc.TypeTc -> Rn.Boundedness -> Tc.AssignOp -> Tc.ExprX Tc.Tc -> DsM Expr
ass name typ binding op xpr = do
    expr <- dsExpr xpr
    ty <- mkClosureType typ
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
