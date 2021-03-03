module CommonTypes (Type (..), Variable(..)) where

import Data.Eq (Eq)
import Data.String (String)
import Prelude (Show)

data Type = Bool | Integer deriving (Eq, Show)

data Variable = Var String Type deriving (Eq, Show)
