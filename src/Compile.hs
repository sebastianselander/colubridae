{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}

module Compile where

import Backend.Desugar.Desugar (desugar)
import Backend.Desugar.Pretty (prettyDesugar)
import Backend.Llvm.Llvm (assemble)
import Backend.Llvm.ToLlvm (llvmOut)
import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Validate (MonadValidate, Validate, mapErrors, runValidate)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Set qualified as Set
import Data.Text (concat)
import Data.Text qualified as Text
import Data.Text.Prettyprint.Doc.Internal.Debug qualified as Diagnose
import Error.Diagnose qualified as Diagnose
import Frontend.Error
import Frontend.Parser.Parse (parse)
import Frontend.Renamer.Pretty (prettyRenamer)
import Frontend.Renamer.Rn (rename)
import Frontend.StatementCheck (check)
import Frontend.Typechecker.Pretty (pThing)
import Frontend.Typechecker.Tc (tc)
import Options (Pass (..))
import Relude hiding (concat, concatMap, intercalate)
import Text.Pretty.Simple (pShow)

data DebugOutput = Debug {phase :: Pass, prettyTxt :: Maybe Text, normalTxt :: Text}
data DebugOutputs = Debugs {debugs :: [DebugOutput], warnings :: [Text]}

instance Semigroup DebugOutputs where
    (<>) (Debugs l1 r1) (Debugs l2 r2) = Debugs (l1 <> l2) (r1 <> r2)

instance Monoid DebugOutputs where
    mempty = Debugs [] []
    mappend = (<>)

log :: (MonadWriter DebugOutputs m) => DebugOutput -> [Text] -> m ()
log debug warnings = do
    tell (Debugs [debug] warnings)

runCompile :: String -> Text -> Either (Diagnose.Diagnostic Text) Text
runCompile fileName fileContents =
    let diagnostic = Diagnose.addFile mempty fileName (Text.unpack fileContents)
     in runValidate
            $ mapErrors (foldl' Diagnose.addReport diagnostic)
            $ compile fileName fileContents

compile :: String -> Text -> Validate [Diagnose.Report Text] Text
compile fileName fileContents = do
    res <- parse fileName fileContents
    (res, names) <- rename res
    -- res <- check res
    pure ""
    -- res <- tc names res
    -- res <- case desugar names res of
    --     res -> do
    --         pure res
    -- case assemble res of
    --     res -> do
    --         pure (llvmOut res)

showDebug :: Set Pass -> DebugOutput -> Text
showDebug dumps (Debug phase pretty normal) =
    if Set.member phase dumps
        then
            unlines
                [ "======== " <> show phase <> " output ========"
                , ""
                , normal
                , fromMaybe "" pretty
                , ""
                ]
        else ""

showDebugs :: Set Pass -> DebugOutputs -> Text
showDebugs dumps (Debugs debugs warnings) = concat (fmap (showDebug dumps) debugs) <> concat warnings
