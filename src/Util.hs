module Util (mapAdjacent, mapFst, mapSnd, formatTuple) where

import Data.List ( tail, zipWith, (++) )
import Control.Applicative((<*>))
import Data.String (String)
import Prelude (Show (..), ($!))

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f = zipWith f <*> tail


mapFst :: (a -> b) -> (a, c) -> (b,c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c,b)
mapSnd f (a, b) = (a, f b)

formatTuple :: (Show a, Show b) => (a, b) -> String -> String
formatTuple (a, b) str = show a ++ str ++ show b