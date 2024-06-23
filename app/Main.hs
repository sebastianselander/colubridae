module Main where

import Data.Text.IO (hPutStrLn)
import Frontend.Parser.Parse
import Frontend.Renamer.Rn (rename)
import Frontend.Typechecker.Tc (tc)
import Relude
import Text.Pretty.Simple
import Types (pPretty)

ok :: Either Text a -> IO a
ok (Left err) = hPutStrLn stderr err *> exitFailure
ok (Right a) = pure a

main :: IO ()
main = do
    putStrLn "Compiling!"
    [file] <- getArgs
    input <- readFileBS file

    putStrLn "\n=== Parse output ===\n"
    res <- ok $ pProgram file (decodeUtf8 input)
    putTextLn (pPretty res)
    pPrint res

    putStrLn "\n=== Rename output ===\n"
    res <- ok $ rename res
    putTextLn (pPretty res)
    pPrint res

    putStrLn "\n=== Tc output ===\n"
    res <- ok $ tc res
    putTextLn (pPretty res)
    pPrint res
