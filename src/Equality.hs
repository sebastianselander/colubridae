module Equality where

import Relude
import Types (ArgX(..))

eqAlphaArg :: forall a b. ArgX a -> ArgX b -> Bool
eqAlphaArg (ArgX _ name1 _) (ArgX _ name2 _) = name1 == name2
