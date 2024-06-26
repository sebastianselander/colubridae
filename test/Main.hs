module Main (main) where

import Compile
import Data.List (groupBy)
import Relude
import System.Directory (listDirectory)
import System.FilePath (dropExtensions)

main :: IO ()
main = do
    goods <-
        ( fmap (\[a, b] -> (a, b))
                . groupBy (\l r -> dropExtensions l == dropExtensions r)
                . sort
            )
            . fmap ("test/good/" ++)
            <$> listDirectory "test/good"

    bads <-
        ( fmap (\[a, b] -> (a, b))
                . groupBy (\l r -> dropExtensions l == dropExtensions r)
                . sort
            )
            . fmap ("test/bad/" ++)
            <$> listDirectory "test/bad"
    goods <- mapM (testFile isRight) goods
    bads <- mapM (testFile isLeft) bads
    unless (and goods && and bads) exitFailure
    exitSuccess

testFile :: (forall a b. Either a b -> Bool) -> (String, String) -> IO Bool
testFile isEither (input, _) = do
    content <- decodeUtf8 <$> readFileBS input
    pure $ isEither $ fst $ runCompile input content
