module BoundedModelChecker
  ( extractDeclarations,
    extractInit,
    extractProperty,
    extractTransition,
  )
where

import CommonTypes
  ( Declaration,
    Type (..),
    Variable (..),
    VariableState (..),
    elemVariableState,
    emptyState,
    lookupBoolVar,
    lookupIntVar,
    shadowElemVariableState,
  )
import Control.Monad
  ( Monad (..),
    sequence,
    void,
  )
import Control.Monad.Free
  ( Free (..),
    liftF,
  )
import Data.Bool
  ( Bool (..),
    otherwise,
    (&&),
    (||),
  )
import Data.Function
  ( flip,
    id,
    ($),
    (.),
  )
import Data.Functor
  ( Functor (..),
    (<$>),
    (<&>),
  )
import Data.Int (Int)
import Data.List
  ( map,
    reverse,
    sum,
    take,
    unlines,
    zip,
    (++),
  )
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.SBV
  ( EqSymbolic (..),
    OrdSymbolic (..),
    Predicate,
    SBV (..),
    SBool (..),
    SInteger (..),
    Symbolic,
    constrain,
    literal,
    sAll,
    sAnd,
    sBools,
    sFalse,
    sIntegers,
    sOr,
    (.&&),
    (.=>),
    (.||),
  )
import Data.SBV.Control (io)
import Data.String (String)
import Data.Tuple (fst)
import ProgramInterpreter
  ( Connective (..),
    EvaluationStackElement (..),
    Program,
    ProgramF (..),
  )
import Util (mapAdjacent)
import Prelude
  ( Num (..),
    Show (show),
    error,
    putStr,
    putStrLn,
    ($!),
  )

extractDeclarations :: Program a -> [Declaration]
extractDeclarations = gatherVariables . gotoState
  where
    gatherVariables (Free (VariableDeclaration name varType next)) = (name, varType) : gatherVariables next
    gatherVariables _ = []

gotoState (Free (Program next)) = gotoState next
gotoState (Free (State next@(Free VariableDeclaration {}))) = next
gotoState x = x

gotoInit = gotoInit' . gotoState
  where
    gotoInit' (Free (VariableDeclaration _ _ next)) = gotoInit' next
    gotoInit' (Free (Init next)) = next
    gotoInit' x = x

gotoTransition = gotoTransition' . gotoInit
  where
    gotoTransition' (Free (Push _ next)) = gotoTransition' next
    gotoTransition' (Free (Transition next)) = next
    gotoTransition' x = x

gotoProperty = gotoProperty' . gotoTransition
  where
    gotoProperty' (Free (Property next)) = next
    gotoProperty' (Free (Push _ next)) = gotoProperty' next
    gotoProperty' x = x

extractInit :: VariableState -> Program a -> SBool
extractInit varState = sAnd . formulaToSBool varState emptyState . gotoInit

extractTransition :: VariableState -> VariableState -> Program a -> SBool
extractTransition previousState currentState = sAnd . formulaToSBool previousState currentState . gotoTransition

extractProperty :: VariableState -> Program a -> SBool
extractProperty varState = sAnd . formulaToSBool varState emptyState . gotoInit . gotoProperty

isArithmetic :: Connective -> Bool
isArithmetic Plus = True
isArithmetic _ = False

isComparative :: Connective -> Bool
isComparative Equals = True
isComparative LessEq = True
isComparative Greater = True
isComparative _ = False

isBoolean :: Connective -> Bool
isBoolean ProgramInterpreter.Implies = True
isBoolean ProgramInterpreter.And = True
isBoolean ProgramInterpreter.Or = True
isBoolean _ = False

arithmetic :: Num a => Connective -> [a] -> a
arithmetic Plus = sum

comparative :: OrdSymbolic a => Connective -> [a] -> SBool
comparative Equals [a, b] = a .== b
comparative LessEq [a, b] = a .<= b
comparative Greater [a, b] = a .> b
comparative conn args = comparative' conn args
  where
    comparative' Equals = allEqual
    comparative' LessEq = sAll id . mapAdjacent (.<=)
    comparative' Greater = sAll id . mapAdjacent (.>)

boolean :: Connective -> [SBool] -> SBool
boolean And [a, b] = a .&& b
boolean Implies [a, b] = a .=> b
boolean Or [a, b] = a .|| b
boolean conn l = boolean' conn l
  where
    boolean' And = sAnd
    boolean' Or = sOr

formulaToSBool :: VariableState -> VariableState -> Program a -> [SBool]
formulaToSBool previousState currentState f@(Free Push {}) = (workoffStack . formulaToStack) f
  where
    genericError stack = "Cannot workoff:\n" ++ (unlines . map show . take 10) stack
    workoffStack' :: [EvaluationStackElement] -> SBool
    workoffStack' (h@(Connect conn arity) : stack)
      | isComparative conn = (fst . workoffComparative [arity] [[]] [conn]) stack
      | isBoolean conn = (fst . workoffBoolean [arity] [[]] [conn]) stack
      | isArithmetic conn = error $! "Top level is arithmetic.\n" ++ genericError stack
    workoffStack' stack = (error . genericError) stack
    workoffArithmetic :: [Int] -> [[SInteger]] -> [Connective] -> [EvaluationStackElement] -> (SInteger, [EvaluationStackElement])
    workoffArithmetic [0] [i] [conn] stack
      | isArithmetic conn = ((arithmetic conn . reverse) i, stack)
      | otherwise = error $! "Connective " ++ show conn ++ " is not arithmetic.\n" ++ genericError stack
    workoffArithmetic (0 : as) (i : inext : is) (c : cs) stack
      | isArithmetic c = workoffArithmetic as ((i' : inext) : is) cs stack
      | otherwise = error $! "Connective " ++ show c ++ " is not arithmetic.\n" ++ genericError stack
      where
        i' = (arithmetic c . reverse) i
    workoffArithmetic (a : as) (i : is) cs ((Connect conn arity) : stack)
      | isArithmetic conn = workoffArithmetic ((a -1) : as) ((i' : i) : is) cs stack'
      | otherwise = error $! "Connective " ++ show conn ++ " is not arithmetic.\n" ++ genericError stack
      where
        (i', stack') = workoffArithmetic [arity] [[]] [conn] stack
    workoffArithmetic (a : as) (i : is) cs (h : stack) = workoffArithmetic ((a -1) : as) ((stackElementToSInteger h : i) : is) cs stack
    workoffComparative :: [Int] -> [[SInteger]] -> [Connective] -> [EvaluationStackElement] -> (SBool, [EvaluationStackElement])
    workoffComparative [0] [i] [conn] stack
      | isComparative conn = ((comparative conn . reverse) i, stack)
      | otherwise = error $! "Connective " ++ show conn ++ " is not comparative.\n" ++ genericError stack
    workoffComparative (0 : as) (i : inext : is) (c : cs) stack = workoffComparative as ((i' : inext) : is) cs stack
      where
        i' = (arithmetic c . reverse) i
    workoffComparative (a : as) (i : is) cs ((Connect conn arity) : stack)
      | isArithmetic conn = workoffComparative ((a -1) : as) ((i' : i) : is) cs stack'
      | otherwise = error $! "Connective " ++ show conn ++ " is not arithmetic.\n" ++ genericError stack
      where
        (i', stack') = workoffArithmetic [arity] [[]] [conn] stack
    workoffComparative (a : as) (i : is) cs (h : stack) = workoffComparative ((a -1) : as) ((stackElementToSInteger h : i) : is) cs stack
    workoffBoolean :: [Int] -> [[SBool]] -> [Connective] -> [EvaluationStackElement] -> (SBool, [EvaluationStackElement])
    workoffBoolean [0] [b] [conn] stack
      | isBoolean conn = ((boolean conn . reverse) b, stack)
      | otherwise = error $! "Connective " ++ show conn ++ " is not boolean.\n" ++ genericError stack
    workoffBoolean (0 : as) (b : bnext : bs) (c : cs) stack = workoffBoolean as ((b' : bnext) : bs) cs stack
      where
        b' = (boolean c . reverse) b
    workoffBoolean (a : as) (b : bs) cs ((Connect conn arity) : stack)
      | isB || isC = workoffBoolean ((a -1) : as) ((b' : b) : bs) cs stack'
      | otherwise = error $! "Connective " ++ show conn ++ " is arithmetic.\n" ++ genericError stack
      where
        isB = isBoolean conn
        isC = isComparative conn
        (b', stack') = if isB then workoffBoolean [arity] [[]] [conn] stack else workoffComparative [arity] [[]] [conn] stack
    workoffBoolean (a : as) (b : bs) cs (h : stack) = workoffBoolean ((a -1) : as) ((stackElementToSBool h : b) : bs) cs stack
    stackElementToSInteger :: EvaluationStackElement -> SInteger
    stackElementToSInteger (IntegerConstant i) = literal i
    stackElementToSInteger (VariableElement (Var name Integer))
      | shadowElemVariableState name currentState = lookupIntVar name currentState
      | otherwise = lookupIntVar name previousState
    stackElementToSInteger _ = literal 0
    stackElementToSBool :: EvaluationStackElement -> SBool
    stackElementToSBool (BoolConstant b) = literal b
    stackElementToSBool (VariableElement (Var name Bool))
      | shadowElemVariableState name currentState = lookupBoolVar name currentState
      | otherwise = lookupBoolVar name previousState

    workoffStack :: [EvaluationStackElement] -> [SBool]
    workoffStack = map (workoffStack' . reverse) . splitStacks
    splitStacks' :: Int -> [EvaluationStackElement] -> [EvaluationStackElement] -> [[EvaluationStackElement]]
    splitStacks' _ worked [] = [worked]
    splitStacks' 0 worked (h@(Connect conn arity) : stack) = worked : splitStacks' arity [h] stack
    splitStacks' n worked (h@(Connect conn arity) : stack) = splitStacks' (n + arity - 1) (h : worked) stack
    splitStacks' n worked (h : stack) = splitStacks' (n - 1) (h : worked) stack
    splitStacks :: [EvaluationStackElement] -> [[EvaluationStackElement]]
    splitStacks (h@(Connect conn arity) : stack) = splitStacks' arity [h] stack

    formulaToStack :: Program a -> [EvaluationStackElement]
    formulaToStack (Free (Push e next)) = e : formulaToStack next
    formulaToStack _ = []
formulaToSBool _ _ _ = []

--bmc :: Int -> Program a ->