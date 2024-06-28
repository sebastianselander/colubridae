{-# LANGUAGE OverloadedStrings #-}

module Compile where

import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Writer (tell, MonadWriter, Writer, runWriter)
import Frontend.Error (Report (..))
import Frontend.Parser.Parse (parse)
import Frontend.Renamer.Rn (rename)
import Frontend.StatementCheck (check)
import Frontend.Typechecker.Tc (tc)
import Relude hiding (concat, intercalate)
import Data.Text (intercalate)
import Text.Pretty.Simple (pShow)
import Frontend.Types (pPretty)

data Phase = Parse | Rename | StCheck | TypeCheck
    deriving Show

data DebugOutput = Debug {phase :: Phase, pretty :: Text, normal :: Text}
data DebugOutputs = Debugs {debugs :: [DebugOutput], warnings :: [Text]}

instance Semigroup DebugOutputs where
  (<>) (Debugs l1 r1) (Debugs l2 r2) = Debugs (l1 <> l2) (r1 <> r2)

instance Monoid DebugOutputs where
  mempty = Debugs [] []
  mappend = (<>)

log :: MonadWriter DebugOutputs m => DebugOutput -> [Text] -> m () 
log debug warnings = do
    tell (Debugs [debug] warnings)

compile :: String -> Text -> ExceptT Text (Writer DebugOutputs) Text
compile fileName fileContents = do
    res <- liftEither $ left report $ parse fileName fileContents
    log (Debug Parse (pPretty res) (toStrict $ pShow res)) []

    res <- liftEither $ left report $ rename res
    log (Debug Rename (pPretty res) (toStrict $ pShow res)) []

    res <- liftEither $ left report $ check res
    log (Debug StCheck (pPretty res) (toStrict $ pShow res)) []

    _ <- case tc res of
        (res, warnings) -> do
            res <- liftEither $ left report res
            log (Debug TypeCheck (pPretty res) (toStrict $ pShow res)) (fmap report warnings)
            pure res

    pure ""

runCompile :: String -> Text -> (Either Text Text, DebugOutputs)
runCompile fileName = runWriter . runExceptT . compile fileName 

showDebug :: DebugOutput -> Text
showDebug (Debug phase pretty normal) = 
    unlines ["======== " <> show phase <> " output ========", "", pretty, "", normal]

showDebugs :: DebugOutputs -> Text
showDebugs (Debugs debugs warnings) = unlines [intercalate "\n" $ fmap showDebug debugs, intercalate "\n" warnings]
