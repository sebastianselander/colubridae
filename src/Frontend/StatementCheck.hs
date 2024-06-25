{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.StatementCheck (check) where

import Control.Lens
import Control.Monad.Validate (MonadValidate, Validate, runValidate)
import Frontend.Error
import Frontend.Renamer.Types
import Relude
import Types

newtype Ctx = Ctx {_inLoop :: Bool}
    deriving (Show)

$(makeLenses ''Ctx)

newtype ChM a = ChM {runCh :: ReaderT Ctx (Validate [ChError]) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadValidate [ChError])

runCheck :: Ctx -> ChM a -> Either [ChError] a
runCheck ctx = runValidate . flip runReaderT ctx . runCh

check :: ProgramRn -> Either [ChError] ProgramRn
check prg@(ProgramX NoExtField defs) = case lefts $ map checkDef defs of
    [] -> pure prg
    xs -> Left $ concat xs

checkDef :: DefRn -> Either [ChError] ()
checkDef (Fn info name _ returnType block) = runCheck (Ctx False) $ case returnType of
    TyLitX NoExtField UnitX -> breakBlock block
    _ -> case block of
        BlockX _ _ (Just _) -> breakBlock block
        _ -> do
            breakBlock block
            returnBlock block >>= \case
                True -> pure ()
                False -> missingReturn info name

breakBlock :: BlockRn -> ChM ()
breakBlock (BlockX _ statements tail) = mapM_ breakStmt statements >> mapM_ breakExpr tail

breakStmt :: StmtRn -> ChM ()
breakStmt = \case
    RetX _ _ -> pure ()
    SBlockX NoExtField block -> breakBlock block
    BreakX info _ -> do
        loop <- view inLoop
        if loop
            then pure ()
            else do
                breakOutsideLoop info
    IfX _ expr true false -> do
        breakExpr expr
        breakBlock true
        mapM_ breakBlock false
    WhileX _ expr block -> breakExpr expr >> locally inLoop (const True) (breakBlock block)
    SExprX NoExtField expr -> breakExpr expr
    StmtX (LoopX _ block) -> locally inLoop (const True) (breakBlock block)

breakExpr :: ExprRn -> ChM ()
breakExpr = \case
    LitX _ _ -> pure ()
    VarX _ _ -> pure ()
    BinOpX _ l _ r -> breakExpr l >> breakExpr r
    AppX _ l rs -> breakExpr l >> mapM_ breakExpr rs
    EStmtX _ stmt -> breakStmt stmt
    LetX _ _ expr -> breakExpr expr
    AssX _ _ _ expr -> breakExpr expr

returnBlock :: BlockRn -> ChM Bool
returnBlock (BlockX _ statements _) = returnStmts statements

returnStmts :: [StmtRn] -> ChM Bool
returnStmts [] = pure False
returnStmts (x : xs) = do
    returnStmt x >>= \case
        False -> returnStmts xs
        True -> case xs of
            [] -> pure True
            _ -> unreachableStatement (hasInfoStmt x) >> pure True

returnStmt :: StmtRn -> ChM Bool
returnStmt = \case
    RetX _ _ -> pure True
    SBlockX NoExtField block -> returnBlock block
    BreakX _ _ -> pure False
    IfX _ expr true false -> do
        if
            | alwaysTrue expr
            , Just (BlockX info _ _) <- false ->
                unreachableStatement info >> returnBlock true
            | alwaysTrue expr -> returnBlock true
            | alwaysFalse expr
            , BlockX info _ _ <- true ->
                unreachableStatement info >> maybe (pure False) returnBlock false
            | otherwise -> (&&) <$> returnBlock true <*> maybe (pure False) returnBlock false
    WhileX _ expr block -> if alwaysTrue expr then returnBlock block else pure False
    SExprX NoExtField _ -> pure False
    StmtX (LoopX _ block) -> returnBlock block

-- TODO: Make mini evaluator
alwaysTrue :: ExprRn -> Bool
alwaysTrue = const False

-- TODO: Make mini evaluator
alwaysFalse :: ExprRn -> Bool
alwaysFalse = const False

hasInfoStmt :: StmtRn -> SourceInfo
hasInfoStmt = \case
    RetX info _ -> info
    SBlockX NoExtField (BlockX info _ _) -> info
    BreakX info _ -> info
    IfX info _ _ _ -> info
    WhileX info _ _ -> info
    SExprX NoExtField expr -> hasInfoExpr expr
    StmtX (LoopX info _) -> info

hasInfoExpr :: ExprRn -> SourceInfo
hasInfoExpr = \case
    LitX info _ -> info
    VarX (info, _) _ -> info
    BinOpX info _ _ _ -> info
    AppX info _ _ -> info
    EStmtX info _ -> info
    LetX (info, _, _) _ _ -> info
    AssX (info, _) _ _ _ -> info
