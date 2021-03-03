module CommonTypes (Type (..)) where

import Data.Eq (Eq)
import Prelude (Show)

data Type = Bool | Integer deriving (Eq, Show)

data Variable = Var String Type deriving (Eq,Show)