module Origin (Origin(..)) where

import Relude
import Data.Data (Data)

data Origin = Top | Lifted
    deriving (Show, Eq, Ord, Data, Generic)
