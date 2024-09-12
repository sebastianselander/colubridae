module Main where

import Compile (runCompile, showDebugs)
import Data.Text.IO (hPutStrLn)
import Relude hiding (null)
import Data.Text (null)
import Options ( Options(..), cmdlineParser )
import System.Process
import qualified Data.Text as Text
import Error.Diagnose qualified as Diagnose

main :: IO ()
main = do
    Options {input, dumps} <- cmdlineParser
    contents <- decodeUtf8 <$> readFileBS input
    case runCompile input contents of
        (Left err) -> do
            Diagnose.printDiagnostic stderr Diagnose.WithUnicode (Diagnose.TabSize 4) Diagnose.defaultStyle err
            exitFailure
        (Right prg) -> do
            writeFileText "out.ll" prg
            let process1 = proc "opt" ["-S", "--O3"]
            optimised <- readCreateProcess process1 (Text.unpack prg)
            writeFileText "out.ll" (Text.pack optimised)
            _ <- createProcess $ proc "lli" ["out.ll"]
            exitSuccess

hPutStrLn' :: Handle -> Text -> IO ()
hPutStrLn' handle str | null str = pure ()
                      | otherwise = hPutStrLn handle str
