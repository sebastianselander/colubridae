{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Llvm.Llvm where

import Backend.Desugar.Types
import Backend.Llvm.Monad
import Backend.Llvm.Types
import Backend.Types
import Control.Lens.Getter (view)
import Control.Lens.Setter (locally)
import Names (Ident (..))
import Relude hiding (Type, and, div, null, or, rem)
import Origin (Origin(..))
import Control.Monad.Extra (concatMapM)

assemble :: Program -> Ir
assemble (Program defs) = Ir <$> runAssembler $ concatMapM assembleDecl (sortBy (comparing Down) defs)

assembleDecl :: Def -> IRBuilder [Decl]
assembleDecl (StaticString name ty text) = pure [GlobalString name ty text]
assembleDecl (Main block) = do
    clearInstructions
    mapM_ assembleExpr block
    pure . LlvmMain <$> extractInstructions
assembleDecl (Fn origin name arguments returnType block) = do
    clearInstructions
    args <- mapM assembleArg arguments
    mapM_ assembleExpr block
    pure . Define origin name args returnType <$> extractInstructions
assembleDecl (TypeSyn name ty) = pure [TypeDefinition name ty]
assembleDecl (Con name ty tyArgs) = do
    clearInstructions 
    assembleCon name ty tyArgs

assembleCon :: Ident -> Type -> Maybe [Type] -> IRBuilder [Decl]
assembleCon name ty = \case
    Nothing -> do
        (_, instrs) <- inContext $ do
            name <- fresh
            operand <- alloca name ty
            ret operand
            -- TODO: (Sebastian, 2024-07-16):
            -- WORKING HERE!!
        pure [Define Constructor name [] ty instrs]
    Just tys -> do
        operands <- mapM (\name -> fmap (LocalReference name) fresh) tys
        pure [Define Constructor name operands ty [Nameless Unreachable]]

assembleArg :: Arg -> IRBuilder Operand
assembleArg (EnvArg ty) = pure $ LocalReference ty (Ident "env")
assembleArg (Arg name ty) = do
    let arg = LocalReference ty (mkArgName name)
    fakeArg <- alloca name ty
    store arg fakeArg
    pure arg

mkArgName :: Ident -> Ident
mkArgName (Ident name) = Ident $ name <> ".arg"

assembleExpr :: TyExpr -> IRBuilder Operand
assembleExpr (Typed taggedType expr) =
    case expr of
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
            eq Bool (ConstantOperand (LBool taggedType False)) expr
        PrefixOp Neg expr -> do
            expr <- assembleExpr expr
            sub taggedType (ConstantOperand (LInt taggedType 0)) expr
        App appExpr argExprs -> do
            app <- assembleExpr appExpr
            args <- mapM assembleExpr argExprs
            call taggedType app args
        Let name varType Nothing -> alloca name varType
        Let name varType (Just expr) -> do
            decl <- alloca name varType
            expr <- assembleExpr expr
            store expr decl
            pure decl
        Ass name varType expr -> do
            expr <- assembleExpr expr
            let operand = LocalReference (ptr varType) name
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
                [] -> pure $ null (ptr opaquePtr)
                _ -> do
                    mem <- malloc (ptr opaquePtr) (i64 (length env * 8))
                    forM_ (zip [0 ..] env) $ \(index, Typed ty expr) -> do
                        gepOperand <- gep mem [i32 @Integer index]
                        comment "Hard coded to 8 size for now, will not work for any type larger at the moment"
                        alloced <- malloc (ptr ty) (i32 $ sizeOf ty) -- TODO: Adapt size to the structure stored
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
        StructIndexing expr n -> do
            operand <- assembleExpr expr
            extractValue operand [fromInteger n]
        ExtractFree bindName envName index -> do
            operand <- gep (localRef (ptr opaquePtr) envName) [i32 index]
            operand <- gep operand [i32 @Integer 0]
            operand <- load (ptr taggedType) operand
            operand <- load taggedType operand
            variable <- alloca bindName taggedType
            store operand variable
            pure variable

assembleLit :: (Monad m) => Lit -> m Operand
assembleLit = \case
    IntLit int -> pure $ ConstantOperand (LInt Int64 int)
    DoubleLit double -> pure $ ConstantOperand (LDouble Float double)
    CharLit char -> pure $ ConstantOperand (LChar Char char)
    BoolLit bool -> pure $ ConstantOperand (LBool Bool bool)
    UnitLit -> pure $ ConstantOperand LUnit
    NullLit -> pure $ ConstantOperand (LNull (PointerType Void))

unit :: (Monad m) => m Operand
unit = pure $ ConstantOperand LUnit

llvmBinOp :: BinOp -> (Type -> Operand -> Operand -> IRBuilder Operand)
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

llvmLit :: Type -> Lit -> Constant
llvmLit ty = \case
    IntLit int -> LInt ty int
    DoubleLit double -> LDouble ty double
    CharLit char -> LChar ty char
    BoolLit bool -> LBool ty bool
    UnitLit -> LUnit
    NullLit -> LNull ty

i32 :: (Integral a) => a -> Operand
i32 = ConstantOperand . LInt Int32 . fromIntegral

i64 :: (Integral a) => a -> Operand
i64 = ConstantOperand . LInt Int64 . fromIntegral
