module Names
    ( Ident (..),
      Names,
      mkNames,
      getOriginalName,
      existName,
      insertName,
      getOriginalName',
      renameBack,
    ) where

import Data.Data (Data)
import Data.Map qualified as Map
import Generics.SYB (everywhere, mkT)
import Prettyprinter (Pretty (..))
import Relude
import Relude.Unsafe (fromJust)

newtype Names = Names {unNames :: Map Ident Ident}
    deriving (Show, Data)

mkNames :: Map Ident Ident -> Names
mkNames = Names

-- Identifier
newtype Ident = Ident Text
    deriving (Show, Eq, Ord, Data, Semigroup, Monoid)

instance Pretty Ident where
    pretty (Ident name) = pretty name

getOriginalName' :: Ident -> Names -> Ident
getOriginalName' name names = fromJust $ Map.lookup name (unNames names)

getOriginalName :: Ident -> Names -> Maybe Ident
getOriginalName name names = Map.lookup name (unNames names)

existName :: Ident -> Names -> Bool
existName name names = Map.member name (unNames names)

insertName :: Ident -> Names -> Names
insertName name names = Names $ Map.insertWith (\_ x -> x) name name (unNames names)

renameBack :: (Data a) => Names -> a -> a
renameBack names = everywhere (mkT f)
  where
    f :: Ident -> Ident
    f name = fromMaybe name (getOriginalName name names)
