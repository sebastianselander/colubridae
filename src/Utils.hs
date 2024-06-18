{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Generics (Data, everything, everywhere, mkQ, mkT)
import Relude

impossible :: Void -> a
impossible _ = error "can not be evaluted"

genMap :: (Data a, Typeable b) => (b -> b) -> a -> a
genMap f = everywhere (mkT f)

listify' :: (Data a, Typeable b) => (b -> Maybe c) -> a -> [c]
listify' f = everything (++) ([] `mkQ` (maybeToList . f))

both :: (Bifunctor p) => (c -> d) -> p c c -> p d d
both f = bimap f f
