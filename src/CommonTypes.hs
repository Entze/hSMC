module CommonTypes (Type (..), Variable (..), VariableState (..), emptyState, fromDeclarations, Declaration) where

import Control.Monad (Monad (..))
import Data.Eq (Eq(..))
import Data.Functor (Functor (..))
import Data.Function ((.))
import Data.List (map, partition, zip, zipWith)
import Data.SBV (EqSymbolic ((.==)), SBool, SInteger, Symbolic, sAnd, sBools, sIntegers, (.&&))
import Data.String (String)
import Data.Tuple (fst, snd)
import Prelude (Show)

data Type = Bool | Integer deriving (Eq, Show)

type Declaration = (String, Type)

data Variable = Var String Type deriving (Eq, Show)

data VariableState = VariableState
  { boolVars :: [(String, SBool)],
    intVars :: [(String, SInteger)]
  }
  deriving (Show, Eq)

emptyState :: VariableState
emptyState = VariableState {boolVars = [], intVars = []}

fromDeclarations :: [Declaration] -> Symbolic VariableState
fromDeclarations declaration =
  do
    sBoolVars <- sBools boolNames
    sIntegerVars <- sIntegers integerNames
    return VariableState {boolVars = zip boolNames sBoolVars, intVars = zip integerNames sIntegerVars}
  where
    integerNames = map fst integers
    boolNames = map fst bools
    (integers, _) = partition ((== Integer) . snd) nonBools
    (bools, nonBools) = partition ((== Bool) . snd) declaration

instance EqSymbolic VariableState where
  VariableState {boolVars = boolVars1, intVars = intVars1} .== VariableState {boolVars = boolVars2, intVars = intVars2} = sAnd (zipWith (.==) (map snd boolVars1) (map snd boolVars2)) .&& sAnd (zipWith (.==) (map snd intVars1) (map snd intVars2))