module Main where

import Parser.Parse
import Relude
import Data.Text.IO (hPutStrLn)
import Types (pPretty)
import Text.Pretty.Simple
import Renamer.Rn (rename)

ok :: Either Text a -> IO a
ok (Left err) = hPutStrLn stderr err *> exitFailure
ok (Right a) = pure a

main :: IO ()
main = do
    putStrLn "Compiling!"
    [file] <- getArgs
    input <- readFileBS file

    res <- ok $ pProgram file (decodeUtf8 input)
    putTextLn (pPretty res)
    pPrint res

    res <- ok $ rename res
    putStrLn "done"
    pPrint res
