module Util where

import Data.List ( tail, zipWith )
import Control.Applicative((<*>))
import Prelude ()

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f = zipWith f <*> tail