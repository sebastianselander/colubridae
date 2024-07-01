module Names (Ident (..), Names, mkNames, getOriginalName, existName, insertName) where

import Data.Data (Data)
import Data.Map qualified as Map
import Relude
import Relude.Unsafe (fromJust)

newtype Names = Names {unNames :: Map Ident Ident}
    deriving (Show, Data)

mkNames :: Map Ident Ident -> Names
mkNames = Names

-- Identifier
newtype Ident = Ident Text
    deriving (Show, Eq, Ord, Data, Semigroup, Monoid)

getOriginalName :: Ident -> Names -> Ident
getOriginalName name names = fromJust $ Map.lookup name (unNames names)

existName :: Ident -> Names -> Bool
existName name names = Map.member name (unNames names)

insertName :: Ident -> Names -> Names
insertName name names = Names $ Map.insertWith (\_ x -> x) name name (unNames names)
