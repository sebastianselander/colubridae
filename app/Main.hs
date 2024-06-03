module Main where

import Parser.Parse
import Relude
import Data.Text.IO (hPutStrLn)
import Parser.Types (pPretty)

main :: IO ()
main = do
    putStrLn "Compiling!"
    [file] <- getArgs
    input <- readFileBS file
    res <- case pProgram file (decodeUtf8 input) of
        Right res -> pure res
        Left err -> hPutStrLn stderr err *> exitFailure
    putTextLn (pPretty res)
