{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module BoundedModelChecker where

import CommonTypes (Type (..), Variable, VariableState (..), emptyState, Declaration)
import Control.Monad (Monad (..), void, sequence)
import Control.Monad.Free (Free (..), liftF)
import Data.Bool (Bool (..))
import Data.Function (flip, ($), (.))
import Data.Functor (Functor (..), (<$>), (<&>))
import Data.Int (Int)
import Data.List (map, zip, (++))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.SBV (SBV (..), SBool (..), SInteger (..), Symbolic, sBools, sIntegers, constrain, EqSymbolic(..), Predicate, sFalse, sAnd)
import Data.SBV.Control(io)
import Data.String (String)
import Data.Tuple (fst)
import Prelude (Show (show), ($!), (+), (-), putStr, putStrLn)
import ProgramInterpreter ( ProgramF(..), Program, EvaluationStackElement(..), Connective(..))


extractDeclarations :: Program a -> [Declaration]
extractDeclarations (Free (Program next)) = gotoState next
  where
    gatherVariables (Free (VariableDeclaration name varType next)) = (name, varType):gatherVariables next
    gatherVariables _ = []
    gotoState (Free (State next@(Free VariableDeclaration {}))) = gatherVariables next
    gotoState _ = []
extractDeclarations _ = []

extractInitPredicate :: VariableState -> Program a -> Predicate
extractInitPredicate varState (Free (Program next)) = gotoInit next
  where
    gotoInit (Free (State next)) = gotoInit next
    gotoInit (Free (VariableDeclaration _ _ next)) = gotoInit next
    gotoInit (Free (Init next)) = (sequence . formulaToPredicate emptyState varState) next <&> sAnd
extractInitPredicate _ _ = return sFalse

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

formulaToPredicate :: VariableState -> VariableState -> Program a -> [Predicate]
formulaToPredicate previousState currentState f@(Free Push {}) = (workoffStack . formulaToStack) f
  where
    workoffStack' :: [EvaluationStackElement] -> Predicate
    workoffStack' _ = return sFalse
    workoffStack :: [EvaluationStackElement] -> [Predicate]
    workoffStack = map workoffStack' . splitStacks
    splitStacks' :: Int -> [EvaluationStackElement] -> [EvaluationStackElement] -> [[EvaluationStackElement]]
    splitStacks' _ worked [] = [worked]
    splitStacks' 0 worked (h@(Connect conn arity):stack) = worked:splitStacks' arity [h] stack
    splitStacks' n worked (h@(Connect conn arity):stack) = splitStacks' (n+arity - 1) (h:worked) stack
    splitStacks' n worked (h:stack) = splitStacks' (n - 1) (h:worked) stack
    splitStacks :: [EvaluationStackElement] -> [[EvaluationStackElement]]
    splitStacks (h@(Connect conn arity):stack) = splitStacks' arity [h] stack

    formulaToStack :: Program a -> [EvaluationStackElement]
    formulaToStack (Free (Push e next)) = e:formulaToStack next
    formulaToStack _ = []
formulaToPredicate _ _ _ = []