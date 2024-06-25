module Main where

import Data.Text.IO (hPutStrLn)
import Frontend.Parser.Parse (parse)
import Frontend.Renamer.Rn (rename)
import Frontend.Typechecker.Tc (tc)
import Relude
import Text.Pretty.Simple
import Types (pPretty)
import Frontend.Error ( Report, report )
import Frontend.StatementCheck (check)

ok :: Report a => Either a b -> IO b
ok (Left err) = hPutStrLn stderr (report err) *> exitFailure
ok (Right a) = pure a

main :: IO ()
main = do
    putStrLn "Compiling!"
    [file] <- getArgs
    input <- readFileBS file

    putStrLn "\n=== Parse output ===\n"
    res <- ok $ parse file (decodeUtf8 input)
    putTextLn (pPretty res)
    pPrint res

    putStrLn "\n=== Rename output ===\n"
    res <- ok $ rename res
    putTextLn (pPretty res)
    pPrint res

    putStrLn "\n=== Return/break check output ===\n"
    res <- ok $ check res
    putTextLn (pPretty res)
    pPrint res

    putStrLn "\n=== Tc output ===\n"
    res <- ok $ fst $ second report $ tc res
    putTextLn (pPretty res)
    pPrint res
