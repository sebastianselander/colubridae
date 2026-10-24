module Backend.Llvm.Llvm2 where

import Data.DList (DList, snoc)
import Relude hiding (Type, and, div, or, rem)

import Data.Map qualified as Map

import Backend.Llvm.Monad (entryLabel)
import Backend.Llvm.Types
    ( ArithOp (..),
      BasicBlock,
      CmpOp (..),
      Constant (..),
      Definition (..),
      Instruction (..),
      Label (L),
      Module (..),
      Named (..),
      Operand (..),
      PartialBlock (..),
      Terminator (Jump, Ret),
      emptyPartialBlock,
      finalizePartialBlock,
      typeOf,
    )
import Backend.Types (Type (..), ptr)
import Impossible (__IMPOSSIBLE__)
import Names (ExplicitName (..), Ident (..), Namespace)

import Backend.Core.Types qualified as Core
import Origin qualified

type Bindings = Map ExplicitName Operand

data IRState = IRState
    { builderBlock :: Maybe PartialBlock
    , bindings :: Bindings
    , nameCounter :: Int
    , namespace :: Namespace
    }

emptyIRState :: Namespace -> IRState
emptyIRState namespace =
    IRState
        { builderBlock = Nothing
        , bindings = mempty
        , nameCounter = 0
        , namespace = namespace
        }

type IRBuilder a = StateT (DList BasicBlock) (ReaderT (IORef IRState) IO) a

lookupBinding :: ExplicitName -> IRBuilder Operand
lookupBinding name = do
    ref <- ask
    fromMaybe __IMPOSSIBLE__ . Map.lookup name . bindings <$> readIORef ref

insertBinding :: ExplicitName -> Operand -> IRBuilder ()
insertBinding name operand = do
    ref <- ask
    modifyIORef ref (\s -> s {bindings = Map.insert name operand (bindings s)})

assemble :: Core.Program -> IO (NonEmpty Module)
assemble (Core.Program modules) = traverse assembleModule modules

assembleModule :: Core.Module -> IO Module
assembleModule (Core.Module namespace defs) = do
    state <- newIORef (emptyIRState namespace)
    defs <- runReaderT (evalStateT (traverse assembleDefinition defs) mempty) state
    pure $ Module defs

assembleDefinition :: Core.Def -> IRBuilder Definition
assembleDefinition def = case def of
    Core.Fn origin name arguments returnType block -> do
        openBlock entryLabel
        arguments <- traverse assembleArgument arguments
        blocks <- assembleExpressions block
        pure (Function origin name arguments returnType blocks)
    Core.Main namespace block -> do
        openBlock entryLabel
        let name = EName namespace "main" "main"
        blocks <- assembleExpressions block
        pure (Function Origin.Function name [] (I 1) blocks)
    _ -> error "TODO"

assembleArgument :: Core.Arg -> IRBuilder Operand
assembleArgument (Core.Arg name ty) = do
    pure (LocalReference ty name)
assembleArgument (Core.EnvArg namespace ty) = pure (LocalReference ty (EName namespace env env))
  where
    env = Ident "env"

assembleExpressions :: [Core.TyExpr] -> IRBuilder [BasicBlock]
assembleExpressions exprs = do
    local id $ do
        traverse_ assembleExpression exprs
        ref <- ask
        blocks <- get
        state <- readIORef ref
        let block = finalizePartialBlock (Nameless (Ret Nothing)) <$> state.builderBlock
        case block of
            Nothing -> pure (toList blocks)
            Just block -> pure (toList $ blocks `snoc` block)

openBlock :: Label -> IRBuilder ()
openBlock lbl = do
    ref <- ask
    modifyIORef ref $ \s -> s {builderBlock = Just $ emptyPartialBlock lbl}

emitBlockStart :: Label -> IRBuilder ()
emitBlockStart lbl = emitBlock >> openBlock lbl

currentBlock :: IRBuilder Label
currentBlock = do
    ref <- ask
    state <- readIORef ref
    case state.builderBlock of
        Nothing -> error "No block has been started"
        Just partialBlock -> pure partialBlock.partialLabel

emitBlock :: IRBuilder ()
emitBlock = do
    ref <- ask
    state <- readIORef ref
    modify (`snoc` finalizePartialBlock (Nameless $ Ret Nothing) state.partialBlock)

assembleExpression :: Core.TyExpr -> IRBuilder Operand
assembleExpression (Core.Typed ty expr) =
    -- To ensure we're always inside a block
    currentBlock >> case expr of
        Core.Constant lit -> do
            pure (assembleLiteral lit)
        Core.Var binding name -> lookupBinding name
        Core.BinOp leftExpr operator rightExpr -> do
            leftExpr <- assembleExpression leftExpr
            rightExpr <- assembleExpression rightExpr
            leftExpr' <- load leftExpr
            rightExpr' <- load rightExpr
            llvmBinOp operator ty leftExpr' rightExpr'
        Core.PrefixOp Core.Deref expr -> undefined
        Core.PrefixOp Core.Ref expr -> undefined
        Core.PrefixOp Core.Not expr -> undefined
        Core.PrefixOp Core.Neg expr -> undefined
        Core.App appExpr argExprs -> undefined
        Core.Let name varType Nothing -> do
            operand <- alloca name varType
            insertBinding name operand
            pure operand
        Core.Let name varType (Just expr) -> do
            operand <- alloca name varType
            value <- assembleExpression expr
            insertBinding name operand
            () <- store value operand
            pure operand
        Core.DerefAss name varType expr -> undefined
        Core.Ass name varType expr -> undefined
        Core.Return expr -> do
            operand <- assembleExpression expr
            v <- load operand
            res <- ret v
            pure (poison (typeOf v))
        Core.Break -> undefined
        Core.If condition trueBlk falseBlk -> undefined
        Core.While condition blk -> do
            let start = L "while_start"
            let continue = L "while_continue"
            let exit = L "while_exit"
            _ <- jump continue
            emitBlockStart continue
            condition <- assembleExpression condition
            undefined
        Core.Closure fun env -> undefined
        Core.StructIndexing expr n -> undefined
        Core.ExtractFree bindName envName index -> undefined
        Core.Match scrutinee@(Core.Typed _ _) matchArms (Core.Catch _name catchExpr) -> undefined
        Core.ToStderrExit var -> undefined

assembleLiteral :: Core.Literal -> Operand
assembleLiteral = \case
    Core.IntLit int -> ConstantOperand (LInt (I 64) int)
    Core.DoubleLit double -> ConstantOperand (LDouble Float double)
    Core.CharLit char -> ConstantOperand (LChar (I 8) char)
    Core.BoolLit bool -> ConstantOperand (LBool (I 1) bool)
    Core.UnitLit -> ConstantOperand LUnit
    Core.NullLit -> ConstantOperand (LNull (PointerType Void))

llvmBinOp :: Core.BinOp -> (Type -> Operand -> Operand -> IRBuilder Operand)
llvmBinOp op = case op of
    Core.MulOp -> mul
    Core.DivOp -> div
    Core.AddOp -> add
    Core.SubOp -> sub
    Core.ModOp -> rem
    Core.LtOp -> lt
    Core.OrOp -> or
    Core.AndOp -> and
    Core.GtOp -> gt
    Core.LteOp -> le
    Core.GteOp -> ge
    Core.EqOp -> eq
    Core.NeqOp -> neq

jump :: Label -> IRBuilder Terminator
jump lbl = emitTerminator (Jump lbl)

ret :: Operand -> IRBuilder Terminator
ret operand = do
    emitTerminator (Ret $ Just operand)

and :: Type -> Operand -> Operand -> IRBuilder Operand
and ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (And ty l r)
    store (LocalReference ty name) operand
    pure operand

unnamed :: Instruction -> IRBuilder ()
unnamed instr = emitInstruction (Nameless instr)

store :: Operand -> Operand -> IRBuilder ()
store lop rop = unnamed (Store lop rop)

or :: Type -> Operand -> Operand -> IRBuilder Operand
or ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Or ty l r)
    store (LocalReference ty name) operand
    pure operand

add :: Type -> Operand -> Operand -> IRBuilder Operand
add = arith LlvmAdd

sub :: Type -> Operand -> Operand -> IRBuilder Operand
sub = arith LlvmSub

mul :: Type -> Operand -> Operand -> IRBuilder Operand
mul = arith LlvmMul

div :: Type -> Operand -> Operand -> IRBuilder Operand
div = arith LlvmDiv

rem :: Type -> Operand -> Operand -> IRBuilder Operand
rem = arith LlvmRem

eq :: Type -> Operand -> Operand -> IRBuilder Operand
eq = cmp LlvmEq

neq :: Type -> Operand -> Operand -> IRBuilder Operand
neq = cmp LlvmNeq

gt :: Type -> Operand -> Operand -> IRBuilder Operand
gt = cmp LlvmGt

lt :: Type -> Operand -> Operand -> IRBuilder Operand
lt = cmp LlvmLt

ge :: Type -> Operand -> Operand -> IRBuilder Operand
ge = cmp LlvmGe

le :: Type -> Operand -> Operand -> IRBuilder Operand
le = cmp LlvmLe

emitTerminator :: Terminator -> IRBuilder Terminator
emitTerminator term = do
    ref <- ask
    state <- readIORef ref
    let builderBlock = state.builderBlock
    case builderBlock of
        Nothing -> pure ()
        Just builderBlock ->
            modifyIORef ref $ \s ->
                s
                    { builderBlock =
                        Just
                            $ builderBlock
                                { partialTerminator = Just $ fromMaybe (Nameless term) builderBlock.partialTerminator
                                }
                    }
    pure term

emitInstruction :: Named Instruction -> IRBuilder ()
emitInstruction instr = do
    ref <- ask
    state <- readIORef ref
    let builderBlock = state.builderBlock
    case builderBlock of
        Nothing -> pure ()
        Just builderBlock ->
            modifyIORef ref $ \s ->
                s
                    { builderBlock =
                        Just
                            $ builderBlock
                                { partialInstructions = builderBlock.partialInstructions `snoc` instr
                                }
                    }

alloca :: ExplicitName -> Type -> IRBuilder Operand
alloca name ty = do
    emitInstruction $ Named name (Alloca ty)
    pure $ LocalReference (ptr ty) name

named :: Instruction -> IRBuilder ExplicitName
named instr = do
    name <- fresh
    emitInstruction $ Named name instr
    pure name

arith :: ArithOp -> Type -> Operand -> Operand -> IRBuilder Operand
arith op ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Arith op ty l r)
    store (LocalReference ty name) operand
    pure operand

fresh :: IRBuilder ExplicitName
fresh = do
    ref <- ask
    state <- readIORef ref
    modifyIORef ref (\s -> s {nameCounter = nameCounter s + 1})
    let ident = Ident $ "_" <> show state.nameCounter
    pure $ EName state.namespace ident ident

cmp :: CmpOp -> Type -> Operand -> Operand -> IRBuilder Operand
cmp op ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Cmp op ty l r)
    store (LocalReference ty name) operand
    pure operand

load :: Operand -> IRBuilder Operand
load operand = LocalReference retty <$> named (Load operand)
  where
    retty = case typeOf operand of
        PointerType ty -> ty
        _ -> error "Cannot load non-pointer operand"

poison :: Type -> Operand
poison = ConstantOperand . Poison

class Value a where
    value :: a -> Either Terminator Operand

instance Value Operand where
    value = Right

instance Value Terminator where
    value = Left
