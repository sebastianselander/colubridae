{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Desugar.Basic (basicDesugar) where

import Backend.Desugar.Types
import Backend.Llvm.Prelude (globalUnit)
import Backend.Types
import Control.Lens (makeLenses)
import Control.Lens.Getter (use, uses, view)
import Control.Lens.Setter (assign, modifying)
import Control.Monad.Extra (concatMapM)
import Data.DList
import Data.List (nub, (\\))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Tuple.Extra (uncurry3)
import Frontend.Renamer.Types qualified as Rn (Boundedness (..))
import Frontend.Typechecker.Types (stmtType, varType)
import Frontend.Typechecker.Types qualified as Tc
import Frontend.Types (NoExtField (NoExtField))
import Frontend.Types qualified as Tc
import Names (Ident (..), Names, existName, insertName)
import Origin (Origin (..))
import Relude hiding (Type, fromList, toList)
import Utils (listify', mapWithIndexM)
import Backend.Llvm.Monad (undef)

data Env = Env
    { _expressions :: DList TyExpr
    , _names :: Names
    , _nameCounter :: Int
    , _lifted :: DList Def
    , _staticStrings :: [(Ident, Type, Text)]
    , _constructorIndex :: Map Ident Int
    }

$(makeLenses ''Env)

newtype DsM a = DsM {runDsm :: StateT Env (Reader (TyExpr -> DsM ())) a}
    deriving (Functor, Applicative, Monad, MonadState Env, MonadReader (TyExpr -> DsM ()))

run :: (TyExpr -> DsM ()) -> Names -> Int -> DsM a -> (a, Env)
run f names n = flip runReader f . flip runStateT (Env mempty names n mempty mempty mempty) . runDsm

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
declare (I 1) = do
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
    defs <- concatMapM dsDef defs
    lifteds <- use lifted
    strings <- use staticStrings
    pure $ Program $ fmap (uncurry3 StaticString) strings <> toList lifteds <> defs

isMain :: Tc.FnTc -> Bool
isMain (Tc.Fn NoExtField (Ident "main") _ _ _) = True
isMain _ = False

dsFunction :: Tc.FnTc -> DsM Def
dsFunction def@(Tc.Fn NoExtField name args returnType (Tc.BlockX (_info, _) stmts tail)) = do
    assign nameCounter 0 -- Start the name counter from 0 for each local scope
    args <- (EnvArg (PointerType Void) :) <$> mapM dsArg args
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
            (I 1) -> pure $ Fn Top name args returnType (toList $ emits `snoc` Typed (I 1) (Return unit))
            _ -> pure $ Fn Top name args returnType (toList emits)

dsDef :: Tc.DefTc -> DsM [Def]
dsDef (Tc.DefFn fn) = pure <$> dsFunction fn
dsDef (Tc.DefAdt adt) = dsAdt adt

dsAdt :: Tc.AdtTc -> DsM [Def]
dsAdt (Tc.AdtX _loc name constructors) = do
    alloc <- constructorAllocSize constructors
    funs <- mapWithIndexM mkConstructorFunction constructors
    case alloc of
        0 -> pure $ TypeSyn name (StructType [Int64]) : funs
        n -> pure $ TypeSyn name (StructType [Int64, ArrayType (n `div` 8) (I 64)]) : funs

mkConstructorFunction :: Int -> Tc.ConstructorTc -> DsM Def
mkConstructorFunction n = \case
    Tc.EnumCons (_loc, ty) name -> do
        modifying constructorIndex (Map.insert name n)
        Con n name <$> dsType ty <*> pure Nothing
    Tc.FunCons (_loc, ty) name tys -> do
        modifying constructorIndex (Map.insert name n)
        Con n name <$> dsType ty <*> Just <$> mapM dsType tys

constructorAllocSize :: (Monad m) => [Tc.ConstructorTc] -> m Int
constructorAllocSize [] = pure 0
constructorAllocSize (Tc.EnumCons (_loc, _) _ : cons) = fmap (max 0) (constructorAllocSize cons)
constructorAllocSize (Tc.FunCons (_loc, _) _ tys : cons) = do
    size <- sum <$> mapM (fmap sizeOf . dsType) tys
    rest <- constructorAllocSize cons
    pure (max (fromIntegral size) rest)

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
        ty <- case binding of
            Rn.Toplevel -> dsType ty
            Rn.Constructor -> dsType ty
            _ -> mkClosureType ty
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
            Typed _ (Var Toplevel _) -> named $ pure $ Typed ty (App l (Typed (PointerType Void) (Lit NullLit) : rs))
            Typed lty l -> do
                let function = StructIndexing (Typed lty l) 0
                let env = StructIndexing (Typed lty l) 1
                named $ pure $ Typed ty (App (Typed lty function) (Typed (PointerType Void) env : rs))
    Tc.LetX info name expr -> do
        (list, expr) <- contextually $ dsExpr expr
        case expr of
            Typed _ (Var Toplevel _) -> do
                varty <- dsType $ view varType info
                let tupleTy = StructType [varty, PointerType Void]
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
                    (EnvArg (PointerType Void) : args)
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
    Tc.MatchX (_loc, ty) scrutinee matchArms -> do
        ty <- mkClosureType ty
        scrutinee <- dsExpr scrutinee
        matchArms <- mapM dsMatchArm matchArms
        pure $ Typed ty $ Match scrutinee matchArms

dsMatchArm :: Tc.MatchArmTc -> DsM MatchArm
dsMatchArm (Tc.MatchArmX _loc pat body) = do
    pat <- dsPat pat
    (emits, body) <- contextually $ dsExpr body
    case toList emits of
        [] -> do
            pure $ MatchArm pat (pure body)
        (x : xs) -> pure $ MatchArm pat (x :| (xs <> [body]))

dsPat :: Tc.PatternTc -> DsM Pattern
dsPat = \case
    Tc.PVarX _loc name -> pure $ PVar name
    Tc.PEnumConX _loc name -> do
        index <- lookupCon name
        pure $ PCon index []
    Tc.PFunConX _loc name nestedPats -> do
        index <- lookupCon name
        let toVar (Tc.PVarX _ name) = name
            toVar _ = error "Internal compiler bug: Nested pattern matching no supported yet"
        pure $ PCon index (fmap toVar nestedPats)

boundArgs :: [Arg] -> [(Type, Ident)]
boundArgs [] = []
boundArgs (x : xs) = case x of
    Arg name ty -> (ty, name) : boundArgs xs
    _ -> boundArgs xs

mkClosureType :: (Monad m) => Tc.TypeTc -> m Type
mkClosureType (Tc.TyFunX NoExtField ls r) = do
    ls <- mapM mkClosureType ls
    r <- mkClosureType r
    pure $ StructType [TyFun ls r, PointerType Void]
mkClosureType (Tc.TypeX (Tc.MutableX ty)) = Mut <$> mkClosureType ty
mkClosureType ty = dsType ty

dsType :: (Monad m) => Tc.TypeTc -> m Type
dsType = \case
    Tc.TyConX NoExtField name -> pure (TyCon name)
    Tc.TyLitX NoExtField Tc.UnitX -> pure Unit
    Tc.TyLitX NoExtField Tc.StringX -> pure (PointerType (I 8))
    Tc.TyLitX NoExtField Tc.CharX -> pure (I 8)
    Tc.TyLitX NoExtField Tc.DoubleX -> pure Float
    Tc.TyLitX NoExtField Tc.IntX -> pure (I 64)
    Tc.TyLitX NoExtField Tc.BoolX -> pure (I 1)
    Tc.TyFunX NoExtField l r -> do
        ls <- mapM dsType l
        r <- dsType r
        pure $ TyFun (PointerType Void : ls) r
    Tc.TypeX (Tc.MutableX ty) -> Mut <$> dsType ty
    Tc.TypeX Tc.AnyX -> pure Unit -- NOTE: `Any` is only the type
    -- of `return` and `break` so that they can be placed anywhere.

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

-- TODO: (Sebastian) Rewrite
dsBlock :: (TyExpr -> DsM ()) -> Ident -> Tc.BlockX Tc.Tc -> DsM [TyExpr]
dsBlock f _ (Tc.BlockX (_, Tc.Unit) stmts tail) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds strings _) =
            run f names' n $ mapM_ dsStmt (stmts <> maybe [] (\x -> [Tc.SExprX NoExtField x]) tail)
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)
dsBlock f _ (Tc.BlockX _ stmts Nothing) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds strings _) = run f names' n $ mapM_ dsStmt stmts
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)
dsBlock f variable (Tc.BlockX (_, ty) stmts (Just tail)) = do
    names' <- use names
    n <- use nameCounter
    let ((), Env emits names'' n' lifteds strings _) =
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
        modifying staticStrings ((name, ArrayType (Text.length string + 1) (I 8), string <> "\\00") :)
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
true = Typed (I 8) $ Lit $ BoolLit True

unit :: TyExpr
unit = Typed (I 1) $ Lit UnitLit

typed :: Tc.TypeTc -> Expr -> DsM TyExpr
typed ty expr = Typed <$> dsType ty <*> pure expr

lookupCon :: Ident -> DsM Int
lookupCon name = uses constructorIndex (fromJust . Map.lookup name)

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
    Rn.Constructor -> Constructor

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
