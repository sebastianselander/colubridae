{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Backend.Llvm.Llvm where

import Backend.Desugar.Types
import Backend.Llvm.Monad
import Backend.Llvm.Types
import Control.Lens.Setter (assign, locally)
import Relude hiding (and, or, rem, div, Type)
import Control.Lens.Getter (view)

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
assembleArg (Arg name ty) = pure $ Variable (llvmType ty) name

assembleExpr :: TyExpr -> IRBuilder Operand
assembleExpr (Typed taggedType' expr) =
    let taggedType = llvmType taggedType'
     in case expr of
            Lit lit -> assembleLit lit
            Var binding name -> do
                case binding of
                    Free -> load taggedType $ Variable (Ptr taggedType) name
                    Bound -> load taggedType $ Variable (Ptr taggedType) name
                    Toplevel -> pure $ Global taggedType name
                    Lambda -> pure $ Global taggedType name
                    Argument -> pure $ Variable taggedType name
            BinOp leftExpr operator rightExpr -> do
                left <- assembleExpr leftExpr
                right <- assembleExpr rightExpr
                llvmBinOp operator taggedType left right
            PrefixOp Not expr -> do
                expr <- assembleExpr expr
                eq I1 (Literal I1 (LBool False)) expr
            PrefixOp Neg expr -> do
                expr <- assembleExpr expr
                sub taggedType (Literal taggedType (LInt 0)) expr
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
                let operand = Variable (Ptr $ llvmType varType) name
                store expr operand
                pure operand
            Return expr -> do
                expr <- assembleExpr expr
                ret expr
                unit
            Break -> do
                lbl <- view breakLabel
                comment "break"
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
                maybe (pure ()) (\falseBlk -> do
                    label false
                    mapM_ assembleExpr falseBlk
                    jump end) falseBlk

                label end

                unit

            While condition blk -> do
                start <- mkLabel "while_start"
                continue <- mkLabel "while_continue"
                exit <- mkLabel "while_exit"
                jump continue
                label continue
                expr <-  assembleExpr condition
                br expr start exit
                label start
                locally breakLabel (const exit) $ mapM_ assembleExpr blk
                jump continue
                label exit
                unit

llvmType :: Type -> LlvmType
llvmType = \case
    Unit -> I1
    String -> Ptr I8
    Char -> I8
    Int -> I64
    Double -> Float
    Bool -> I1
    Mut ty -> llvmType ty
    TyFun args ret -> FunPtr (llvmType ret) (fmap llvmType args)

assembleLit :: (Monad m) => Lit -> m Operand
assembleLit = \case
    IntLit int -> pure $ Literal I64 (LInt int)
    DoubleLit double -> pure $ Literal Float (LDouble double)
    StringLit _ -> error "TODO" -- BUG: string literals should be lifted earlier
    CharLit char -> pure $ Literal I8 (LChar char)
    BoolLit bool -> pure $ Literal I1 (LBool bool)
    UnitLit -> pure $ Literal I1 LUnit

unit :: Monad m => m Operand
unit = pure $ Literal I1 LUnit

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
