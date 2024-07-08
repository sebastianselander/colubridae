{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Llvm.Llvm where

import Backend.Desugar.Types
import Backend.Llvm.Monad
import Backend.Llvm.Types
import Control.Lens.Getter (view)
import Control.Lens.Setter (assign, locally)
import Relude hiding (null, Type, and, div, or, rem)

assemble :: Program -> Ir
assemble (Program defs) = Ir <$> runAssembler $ mapM assembleDecl defs

-- NOTE: Very imperative
assembleDecl :: Def -> IRBuilder Decl
assembleDecl (Main block) = do
    assign instructions emptyInstructions
    mapM_ assembleExpr block
    LlvmMain <$> extractInstructions
assembleDecl (Fn origin name arguments returnType' block) = do
    assign instructions emptyInstructions
    args <- mapM assembleArg arguments
    let returnType = llvmType returnType'
    mapM_ assembleExpr block
    Define origin name args returnType <$> extractInstructions

assembleArg :: (Monad m) => Arg -> m Operand
assembleArg (Arg name ty) = pure $ LocalReference (llvmType ty) name

assembleExpr :: TyExpr -> IRBuilder Operand
assembleExpr (Typed taggedType' expr) =
    let taggedType = llvmType taggedType'
     in case expr of
            Lit lit -> assembleLit lit
            Var binding name -> do
                case binding of
                    Free -> load taggedType $ LocalReference (ptr taggedType) name
                    Bound -> load taggedType $ LocalReference (ptr taggedType) name
                    Toplevel -> pure $ global taggedType name
                    GlblConst -> do
                        -- NOTE: This will not work with global strings
                        op <- gep (global (ptr taggedType) name) [i32 0]
                        load taggedType op
                    Argument -> pure $ LocalReference taggedType name
                    Lambda -> undefined
            BinOp leftExpr operator rightExpr -> do
                left <- assembleExpr leftExpr
                right <- assembleExpr rightExpr
                llvmBinOp operator taggedType left right
            PrefixOp Not expr -> do
                expr <- assembleExpr expr
                eq I1 (ConstantOperand (LBool taggedType False)) expr
            PrefixOp Neg expr -> do
                expr <- assembleExpr expr
                sub taggedType (ConstantOperand (LInt taggedType 0)) expr
            App appExpr argExprs -> do
                app <- assembleExpr appExpr
                args <- mapM assembleExpr argExprs
                call taggedType app args
            Let name varType Nothing -> alloca name (llvmType varType)
            Let name varType (Just expr) -> do
                decl <- alloca name (llvmType varType)
                expr <- assembleExpr expr
                store expr decl
                pure decl
            Ass name varType expr -> do
                expr <- assembleExpr expr
                let operand = LocalReference (ptr $ llvmType varType) name
                store expr operand
                pure operand
            Return expr -> do
                expr <- assembleExpr expr
                ret expr
                unit
            Break -> do
                lbl <- view breakLabel
                jump lbl
                unit
            If condition trueBlk falseBlk -> do
                true <- mkLabel "if_true"
                false <- mkLabel "if_false"
                end <- mkLabel "if_after"
                condition <- assembleExpr condition
                -- jump to end if no else branch exist
                br condition true (maybe end (const false) falseBlk)
                -- true case
                label true
                mapM_ assembleExpr trueBlk
                jump end

                -- false case, if there is one
                maybe
                    (pure ())
                    ( \falseBlk -> do
                        label false
                        mapM_ assembleExpr falseBlk
                        jump end
                    )
                    falseBlk

                label end

                unit
            While condition blk -> do
                start <- mkLabel "while_start"
                continue <- mkLabel "while_continue"
                exit <- mkLabel "while_exit"
                jump continue
                label continue
                expr <- assembleExpr condition
                br expr start exit
                label start
                locally breakLabel (const exit) $ mapM_ assembleExpr blk
                jump continue
                label exit
                unit
            Closure fun env -> do
                name <- fresh
                declaration <- alloca name taggedType
                functionPointer <- gep declaration [i64 0]
                environmentPointer <- gep declaration [i64 1]
                functionOperand <- assembleExpr fun
                store functionOperand functionPointer
                store (null (PointerType LlvmVoid)) environmentPointer
                -- TODO: Store all free variables in the malloc
                -- TODO: 1. Calculate size needed for malloc
                load taggedType declaration
            PtrIndexing expr n -> do
                -- TODO: Use this only for free varibles
                operand <- assembleExpr expr
                gep operand [i32 n]
            StructIndexing expr n  -> do
                operand <- assembleExpr expr
                extractValue operand [fromInteger n]


llvmType :: Type -> LlvmType
llvmType = \case
    Void -> LlvmVoid
    Unit -> I1
    String -> PointerType I8
    Char -> I8
    Int -> I64
    Double -> Float
    Bool -> I1
    Mut ty -> llvmType ty
    TyFun args ret -> FunPtr (llvmType ret) (fmap llvmType args)
    Tuple tys -> ArrayType (fmap llvmType tys)
    Ptr ty -> PointerType (llvmType ty)

assembleLit :: (Monad m) => Lit -> m Operand
assembleLit = \case
    IntLit int -> pure $ ConstantOperand (LInt I64 int)
    DoubleLit double -> pure $ ConstantOperand (LDouble Float double)
    CharLit char -> pure $ ConstantOperand (LChar I8 char)
    BoolLit bool -> pure $ ConstantOperand (LBool I1 bool)
    UnitLit -> pure $ ConstantOperand LUnit
    NullLit -> pure $ ConstantOperand (LNull (PointerType LlvmVoid))

unit :: (Monad m) => m Operand
unit = pure $ ConstantOperand LUnit

llvmBinOp :: BinOp -> (LlvmType -> Operand -> Operand -> IRBuilder Operand)
llvmBinOp op = case op of
    Mul -> mul
    Div -> div
    Add -> add
    Sub -> sub
    Mod -> rem
    Lt -> lt
    Backend.Desugar.Types.Or -> or
    Backend.Desugar.Types.And -> and
    Gt -> gt
    Lte -> le
    Gte -> ge
    Eq -> eq
    Neq -> neq

llvmLit :: LlvmType -> Lit -> Constant
llvmLit ty = \case
    IntLit int -> LInt ty int
    DoubleLit double -> LDouble ty double
    CharLit char -> LChar ty char
    BoolLit bool -> LBool ty bool
    UnitLit -> LUnit
    NullLit -> LNull ty

i32 :: Integer -> Operand
i32 = ConstantOperand . LInt I32

i64 :: Integer -> Operand
i64 = ConstantOperand . LInt I64
