{-# LANGUAGE DeriveFunctor #-}
module CommonTypes (Type (..), Variable (..), VariableState (..)) where

import Data.Eq (Eq)
import Data.List (map, zipWith)
import Data.SBV (EqSymbolic ((.==)), SBool, SInteger, sAnd, (.&&))
import Data.String (String)
import Data.Tuple (fst, snd)
import Data.Functor (Functor (..))
import Prelude (Show)

data Type = Bool | Integer deriving (Eq, Show)

data Variable = Var String Type deriving (Eq, Show)

data VariableState = VariableState
  { boolVars :: [(String, SBool)],
    intVars :: [(String, SInteger)]
  }
  deriving (Show, Eq)

instance EqSymbolic VariableState where
  VariableState {boolVars = boolVars1, intVars = intVars1} .== VariableState {boolVars = boolVars2, intVars = intVars2} = sAnd (zipWith (.==) (map snd boolVars1) (map snd boolVars2)) .&& sAnd (zipWith (.==) (map snd intVars1) (map snd intVars2))