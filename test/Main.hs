{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Compile
import Data.List (groupBy)
import Relude
import System.Directory (listDirectory)
import System.FilePath (dropExtensions)
import Data.Text (pack, unpack)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
    putStrLn "RUNNING TESTS\n"
    goods <-
        ( fmap (\case
            [a,b] -> (a,b)
            [a] -> error $ "missing file for: " <> pack a
            _ -> error "incorrect amount of files"
            )
                . groupBy (\l r -> dropExtensions l == dropExtensions r)
                . sort
            )
            . fmap ("test/good/" ++)
            <$> listDirectory "test/good"

    bads <-
        sort
            . fmap ("test/bad/" ++)
            <$> listDirectory "test/bad"
    goods <- mapM (testFile isRight . second Just) goods
    bads <- mapM (testFile isLeft . (,Nothing)) bads
    unless (and goods && and bads) exitFailure
    exitSuccess

testFile :: (forall a b. Either a b -> Bool) -> (String, Maybe String) -> IO Bool
testFile _ (input, Just output) = do
    content <- decodeUtf8 <$> readFileBS input
    let (a,_) = runCompile input content
    putStrLn "=========================================================="
    case a of
        Left err -> putStrLn ("Test: '" <> input <> "' failed with message: " <> unpack err) >> pure False
        Right res -> do
            content <- decodeUtf8 <$> readFileBS output
            putStrLn ("Running test for '" <> input <> "'")
            (_, res, err) <- readProcessWithExitCode "lli" [] (unpack res)
            if content == res
               then putStrLn ("Success for '" <> input <> "'") >> pure True
               else do
                   putStrLn $ "Expected: " <> onEmpty content
                   putStrLn $ "Got: " <> onEmpty res
                   putStrLn ("Test: '" <> input <> "' failed with error message: " <> err) >> pure False
testFile isEither (input, Nothing) = do
    content <- decodeUtf8 <$> readFileBS input
    let (a,_) = runCompile input content
    if isEither a
       then putStrLn ("Success for '" <> input <> "'") >> pure True
       else putStrLn ("Test: '" <> input <> "' failed.") >> pure False

onEmpty :: String -> String
onEmpty "" = "<empty>"
onEmpty s = s
