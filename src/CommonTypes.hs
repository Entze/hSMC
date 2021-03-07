{-# LANGUAGE NamedFieldPuns #-}

module CommonTypes
  ( Type (..),
    Variable (..),
    VariableState (..),
    emptyState,
    fromDeclarations,
    createFromDeclarations,
    Declaration,
    lookupBoolVar,
    lookupIntVar,
    elemVariableState,
    shadowElemVariableState,
    extractValues
  )
where

import Control.Monad (Monad (..), mapM)
import Data.Bool (Bool (..), not, otherwise, (&&), (||))
import Data.Eq (Eq (..))
import Data.Function ((.))
import Data.Functor (Functor (..))
import Data.List as List (elem, elemIndex, filter, init, last, map, partition, unzip, zip, zipWith, (!!))
import Data.Maybe (Maybe (..), isNothing, maybe)
import Data.SBV (EqSymbolic ((.==)), SBool, SInteger, Symbolic, literal, sAnd, sBools, sFalse, sIntegers, (.&&))
import Data.SBV.Control (Query, freshVar, getValue)
import Data.SBV.Trans (forall)
import Data.String (String)
import Data.Tuple (fst, snd)
import Util (mapFst, mapSnd)
import Prelude (Show, Integer(..))

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

elemVariableState :: String -> VariableState -> Bool
elemVariableState name VariableState {boolVars, intVars} = name `elem` boolNames || name `elem` intNames
  where
    (boolNames, _) = unzip boolVars
    (intNames, _) = unzip intVars

shadowElemVariableState :: String -> VariableState -> Bool
shadowElemVariableState name varState = shadow == '_' && elemVariableState name' varState && not (elemVariableState name varState)
  where
    (name', shadow) = (init name, last name)

lookupBoolVar :: String -> VariableState -> SBool
lookupBoolVar str VariableState {boolVars, intVars}
  | last str == '_' && isNothing indexOf = maybe sFalse (vars !!) indexOf'
  | otherwise = maybe sFalse (vars !!) indexOf
  where
    (names, vars) = unzip boolVars
    indexOf = List.elemIndex str names
    indexOf' = List.elemIndex deshadowed names
    deshadowed = List.init str

lookupIntVar :: String -> VariableState -> SInteger
lookupIntVar str VariableState {boolVars, intVars}
  | last str == '_' && isNothing indexOf = maybe (literal 0) (vars !!) indexOf'
  | otherwise = maybe (literal 0) (vars !!) indexOf
  where
    (names, vars) = unzip intVars
    indexOf = elemIndex str names
    indexOf' = elemIndex deshadowed names
    deshadowed = List.init str

fromDeclarations :: [Declaration] -> Symbolic VariableState
fromDeclarations declaration =
  do
    sBoolVars <- sBools boolNames
    sIntegerVars <- sIntegers integerNames
    return VariableState {boolVars = zip boolNames sBoolVars, intVars = zip integerNames sIntegerVars}
  where
    (boolNames, integerNames) = unpackDeclarations declaration

createFromDeclarations :: [Declaration] -> Query VariableState
createFromDeclarations declaration =
  do
    qBoolVars <- mapM freshVar boolNames
    qIntegerVars <- mapM freshVar integerNames
    return VariableState {boolVars = zip boolNames qBoolVars, intVars = zip integerNames qIntegerVars}
  where
    (boolNames, integerNames) = unpackDeclarations declaration


extractValues :: VariableState -> Query ([(String, Bool)], [(String, Integer)])
extractValues VariableState{boolVars, intVars} 
  = do
    boolValues <- mapM getValue bools
    integerValues <- mapM getValue integers
    return (zip boolNames boolValues, zip integerNames integerValues)
  where
    (boolNames, bools) = unzip boolVars
    (integerNames, integers) = unzip intVars

unpackDeclarations :: [Declaration] -> ([String], [String])
unpackDeclarations declaration = (boolNames, integerNames)
  where
    integerNames = map fst integers
    boolNames = map fst bools
    (integers, _) = partition ((== Integer) . snd) nonBools
    (bools, nonBools) = partition ((== Bool) . snd) declaration

instance EqSymbolic VariableState where
  VariableState {boolVars = boolVars1, intVars = intVars1} .== VariableState {boolVars = boolVars2, intVars = intVars2} = sAnd (zipWith (.==) (map snd boolVars1) (map snd boolVars2)) .&& sAnd (zipWith (.==) (map snd intVars1) (map snd intVars2))