module Rename where

import Names (Names, Ident, getOriginalName)
import Generics.SYB
import Frontend.Types (ExprX, ForallX)
import Relude

renameBack :: (Data (ExprX a), ForallX Data a) => Names -> ExprX a -> ExprX a
renameBack names = everywhere (mkT f)
  where
    f :: Ident -> Ident
    f name = fromMaybe name (getOriginalName name names)
