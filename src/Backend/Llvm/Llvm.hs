{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Llvm.Llvm where

import Backend.Desugar.Types
import Backend.Llvm.Monad
import Backend.Llvm.Types
import Control.Lens.Getter (view)
import Control.Lens.Setter (assign, locally)
import Relude hiding (null, Type, and, div, or, rem)
import Names (Ident (..))

assemble :: Program -> Ir
assemble (Program defs) = Ir <$> runAssembler $ mapM assembleDecl defs

-- NOTE: Very imperative
assembleDecl :: Def -> IRBuilder Decl
assembleDecl (StaticString name ty text) = pure $ GlobalString name (llvmType ty) text
assembleDecl (Main block) = do
    assign instructions emptyInstructions
    mapM_ assembleExpr block
    LlvmMain <$> extractInstructions
assembleDecl (Fn origin name arguments returnType' block) = do
    assign instructions emptyInstructions
    let returnType = llvmType returnType'
    args <- mapM assembleArg arguments
    mapM_ assembleExpr block
    Define origin name args returnType <$> extractInstructions

assembleArg :: Arg -> IRBuilder Operand
assembleArg (EnvArg ty) = pure $ LocalReference (llvmType ty) (Ident "env")
assembleArg (Arg name ty) = do
    let arg = LocalReference (llvmType ty) (mkArgName name)
    fakeArg <- alloca name (llvmType ty)
    store arg fakeArg
    pure arg

mkArgName :: Ident -> Ident
mkArgName (Ident name) = Ident $ name <> ".arg"

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
                        op <- gep (global (ptr taggedType) name) [i32 @Integer 0]
                        load taggedType op
                    Argument -> pure $ LocalReference taggedType name
                    Lambda -> load taggedType $ LocalReference (ptr taggedType) name
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
                -- TODO: Test more with types of different sizes
                name <- fresh
                mem <- case env of
                    [] -> pure $ null (ptr blindPtr)
                    _ -> do
                        mem <- malloc (ptr blindPtr) (i64 (length env * 8))
                        forM_ (zip [0..] env) $ \(index, Typed ty expr) -> do
                            gepOperand <- gep mem [i32 @Integer index]
                            name <- fresh
                            alloced <- alloca name (llvmType ty)
                            operand <- assembleExpr (Typed ty expr)
                            store operand alloced
                            store alloced gepOperand
                        pure mem
                declaration <- alloca name taggedType
                functionPointer <- gep declaration [i32 @Integer 0, i32 @Integer 0]
                environmentPointer <- gep declaration [i32 @Integer 0, i32 @Integer 1]
                functionOperand <- assembleExpr fun
                store functionOperand functionPointer
                store mem environmentPointer
                load taggedType declaration
            StructIndexing expr n  -> do
                operand <- assembleExpr expr
                extractValue operand [fromInteger n]
            ExtractFree bindName envName index -> do
                operand <- gep (localRef (ptr blindPtr) envName) [i32 index]
                operand <- load (ptr taggedType) operand
                operand <- load taggedType operand
                variable <- alloca bindName taggedType 
                store operand variable
                pure variable

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
    Struct tys -> StructType (fmap llvmType tys)
    Array size ty -> ArrayType (fromIntegral size) (llvmType ty)
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

i32 :: Integral a => a -> Operand
i32 = ConstantOperand . LInt I32 . fromIntegral

i64 :: Integral a => a -> Operand
i64 = ConstantOperand . LInt I64 . fromIntegral
