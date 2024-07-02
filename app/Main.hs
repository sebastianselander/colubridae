module Main where

import Compile (runCompile, showDebugs)
import Data.Text.IO (hPutStrLn)
import Relude

main :: IO ()
main = do
    [fileName] <- getArgs
    contents <- decodeUtf8 <$> readFileBS fileName

    case runCompile fileName contents of
        (Left err, debugs) -> do
            hPutStrLn stderr $ showDebugs debugs
            hPutStrLn stderr err
            exitFailure
        (Right prg, debugs) -> do
            hPutStrLn stderr $ showDebugs debugs
            writeFileText "out.ll" prg
            exitSuccess
