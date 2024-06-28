{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Generics (Data, everything, everywhere, mkQ, mkT)
import Relude
import qualified Data.Text as Text

genMap :: (Data a, Typeable b) => (b -> b) -> a -> a
genMap f = everywhere (mkT f)

listify' :: (Data a, Typeable b) => (b -> Maybe c) -> a -> [c]
listify' f = everything (++) ([] `mkQ` (maybeToList . f))

both :: (Bifunctor p) => (c -> d) -> p c c -> p d d
both f = bimap f f

chain :: (a -> a -> b) -> a -> [a] -> [b]
chain _ _ [] = []
chain f base [x] = [f base x]
chain f base (x:y:xs) = f base x : go (x:y:xs)
  where
    go (x:y:xs) = f x y : go (y:xs)
    go _ = []


indent :: Int -> Text -> Text
indent n t = Text.replicate n " " <> t

quote :: Text -> Text
quote txt = "'" <> txt <> "'"
