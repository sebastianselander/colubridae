module Origin (Origin(..)) where

import Relude
import Data.Data (Data)

data Origin = Top | Lifted | ConstructorFn
    deriving (Show, Eq, Ord, Data, Generic)
