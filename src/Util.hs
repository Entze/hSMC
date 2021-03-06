module Util (mapAdjacent, mapFst, mapSnd) where

import Data.List ( tail, zipWith )
import Control.Applicative((<*>))
import Prelude ()

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f = zipWith f <*> tail


mapFst :: (a -> b) -> (a, c) -> (b,c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c,b)
mapSnd f (a, b) = (a, f b)