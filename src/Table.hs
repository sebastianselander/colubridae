{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Table where

import Control.Lens.TH
import Relude hiding (Type)

import Frontend.Builtin (Builtins)
import Frontend.Types (Forall)
import Names (ExplicitName, Namespace)


data DefTable phase a info = Table
    { _builtIns :: Builtins phase
    , _functions :: Map Namespace (Map ExplicitName (a, info))
    , _types :: Map Namespace (Map ExplicitName (a, info))
    , _constructors :: Map Namespace (Map ExplicitName (a, info))
    }


deriving instance (Forall Show phase, Show a, Show info) => Show (DefTable phase a info)


$(makeLenses ''DefTable)
