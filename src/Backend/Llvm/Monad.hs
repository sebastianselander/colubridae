{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Llvm.Monad where

import Backend.Llvm.Types
import Control.Lens (makeLenses)
import Control.Lens.Getter (use, uses)
import Control.Lens.Setter (modifying, (+=))
import Data.DList (DList, snoc)
import Names (Ident (..))
import Relude
import qualified Data.Set as Set

data IRBuilderState = IRBuilderState
    { _instructions :: DList (Named Instruction)
    , _varCounter :: !Word16
    , _labelCounter :: !Word16
    , _labelReserved :: Set Ident
    }
    deriving (Show)

newtype IRBuilderCtx = IRBuilderCtx {
    _breakLabel :: Label
}
$(makeLenses ''IRBuilderState)
$(makeLenses ''IRBuilderCtx)

emptyInstructions :: DList (Named Instruction)
emptyInstructions = mempty


initialIRBuilderState :: IRBuilderState
initialIRBuilderState =
    IRBuilderState
        { _instructions = mempty
        , _varCounter = 0
        , _labelCounter = 1
        , _labelReserved = mempty
        }

initialIRBuilderCtx :: IRBuilderCtx
initialIRBuilderCtx = IRBuilderCtx {_breakLabel=L $ Ident "initial_label"}

newtype IRBuilder a = IRBuilder {runBuilder :: StateT IRBuilderState (Reader IRBuilderCtx) a}
    deriving (Functor, Applicative, Monad, MonadState IRBuilderState, MonadReader IRBuilderCtx)

extractInstructions :: IRBuilder [Named Instruction]
extractInstructions = uses instructions toList

runAssembler :: IRBuilder a -> a
runAssembler = flip runReader initialIRBuilderCtx . flip evalStateT initialIRBuilderState . runBuilder

emit :: Named Instruction -> IRBuilder ()
emit instr = modifying instructions (`snoc` instr)

fresh :: (MonadState IRBuilderState m) => m Ident
fresh = do
    n <- use varCounter
    varCounter += 1
    pure $ Ident ("_" <> show n)

call :: LlvmType -> Operand -> [Operand] -> IRBuilder Operand
call ty function args = LocalReference ty <$> named (Call ty function args)

voidCall :: Operand -> [Operand] -> IRBuilder ()
voidCall function args = emit $ Nameless $ Call I1 function args

add :: LlvmType -> Operand -> Operand -> IRBuilder Operand
add = arith LlvmAdd

sub :: LlvmType -> Operand -> Operand -> IRBuilder Operand
sub = arith LlvmSub

mul :: LlvmType -> Operand -> Operand -> IRBuilder Operand
mul = arith LlvmMul

div :: LlvmType -> Operand -> Operand -> IRBuilder Operand
div = arith LlvmDiv

rem :: LlvmType -> Operand -> Operand -> IRBuilder Operand
rem = arith LlvmRem

arith :: ArithOp -> LlvmType -> Operand -> Operand -> IRBuilder Operand
arith op ty l r = LocalReference ty <$> named (Arith op ty l r)

named :: Instruction -> IRBuilder Ident
named instr = do
    name <- fresh
    emit $ Named name instr
    pure name

unnamed :: Instruction -> IRBuilder ()
unnamed instr = emit (Nameless instr)

cmp :: CmpOp -> LlvmType -> Operand -> Operand -> IRBuilder Operand
cmp op ty l r = LocalReference ty <$> named (Cmp op ty l r)

eq :: LlvmType -> Operand -> Operand -> IRBuilder Operand
eq = cmp LlvmEq

neq :: LlvmType -> Operand -> Operand -> IRBuilder Operand
neq = cmp LlvmNeq

gt :: LlvmType -> Operand -> Operand -> IRBuilder Operand
gt = cmp LlvmGt

lt :: LlvmType -> Operand -> Operand -> IRBuilder Operand
lt = cmp LlvmLt

ge :: LlvmType -> Operand -> Operand -> IRBuilder Operand
ge = cmp LlvmGe

le :: LlvmType -> Operand -> Operand -> IRBuilder Operand
le = cmp LlvmLe

and :: LlvmType -> Operand -> Operand -> IRBuilder Operand
and ty l r = LocalReference ty <$> named (And ty l r)

or :: LlvmType -> Operand -> Operand -> IRBuilder Operand
or ty l r = LocalReference ty <$> named (Or ty l r)

alloca :: Ident -> LlvmType -> IRBuilder Operand
alloca name ty = do
    emit $ Named name (Alloca ty)
    pure $ LocalReference (ptr ty) name

malloc :: Operand -> IRBuilder Operand
malloc operand = do
    name <- fresh
    emit $ Named name (Malloc operand)
    pure $ LocalReference (ptr (typeOf operand)) name

store :: Operand -> Operand -> IRBuilder ()
store lop rop = unnamed (Store lop rop)

load :: LlvmType -> Operand -> IRBuilder Operand
load ty operand = LocalReference ty <$> named (Load operand)

ret :: Operand -> IRBuilder ()
ret operand = unnamed (Ret operand)

label :: Label -> IRBuilder ()
label lbl = do
    emit (Nameless (Label lbl))

-- TODO: Fix numbering
mkLabel :: Text -> IRBuilder Label
mkLabel desc = do
    n <- use labelCounter
    labelCounter += 1
    let lbl = Ident $ desc <> "_" <> show n
    used <- use labelReserved
    if lbl `Set.member` used
       then mkLabel desc
       else modifying labelReserved (Set.insert lbl) >> pure (L lbl)

comment :: Text -> IRBuilder ()
comment cmnt = emit (Nameless (Comment cmnt))

br :: Operand -> Label -> Label -> IRBuilder ()
br operand leftLbl rightLbl = unnamed (Br operand leftLbl rightLbl)

jump :: Label -> IRBuilder ()
jump lbl = unnamed (Jump lbl)

-- | Does not work correctly for structure if gep is used nested
gep :: Operand -> [Operand] -> IRBuilder Operand
gep op ops = LocalReference (typeOf op) <$> named (GetElementPtr op ops)

extractValue :: Operand -> [Word32] -> IRBuilder Operand
extractValue operand indices = LocalReference (extractValueType (typeOf operand) indices) <$> named (ExtractValue operand indices)

extractValueType :: LlvmType -> [Word32] -> LlvmType
extractValueType ty [] = ty
extractValueType ty (x:xs) = case ty of
    ArrayType tys -> case maybeAt (fromIntegral x) tys of
        Nothing -> error "Extract value: indexing outside structure"
        Just ty -> extractValueType ty xs
    ty -> error $ "Extract value: indexing in non-indexable structure" <> show ty


ptr :: LlvmType -> LlvmType
ptr = PointerType

global :: LlvmType -> Ident -> Operand
global ty = ConstantOperand . GlobalReference ty

null :: LlvmType -> Operand
null ty = ConstantOperand (LNull ty)
