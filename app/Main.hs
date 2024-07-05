module Main where

import Compile (runCompile, showDebugs)
import Data.Text.IO (hPutStrLn)
import Relude hiding (null)
import Data.Text (null)
import Options ( Options(..), cmdlineParser )
main :: IO ()
main = do
    Options {input, dumps} <- cmdlineParser
    contents <- decodeUtf8 <$> readFileBS input
    case runCompile input contents of
        (Left err, debugs) -> do
            hPutStrLn' stderr $ showDebugs dumps debugs
            hPutStrLn' stderr err
            exitFailure
        (Right prg, debugs) -> do
            hPutStrLn' stderr $ showDebugs dumps debugs
            writeFileText "out.ll" prg
            exitSuccess

hPutStrLn' :: Handle -> Text -> IO ()
hPutStrLn' handle str | null str = pure ()
                      | otherwise = hPutStrLn handle str
