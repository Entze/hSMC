{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module BoundedModelChecker where

import CommonTypes (Type (..), Variable, VariableState (..))
import Control.Monad (Monad (..), void)
import Control.Monad.Free (Free (..), liftF)
import Data.Bool (Bool (..))
import Data.Function (flip, ($), (.))
import Data.Functor (Functor (..), (<$>))
import Data.Int (Int)
import Data.List (map, zip, (++))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.SBV (SBV (..), SBool (..), SInteger (..), Symbolic, sBools, sIntegers, constrain, EqSymbolic(..))
import Data.SBV.Control(io)
import Data.String (String)
import Data.Tuple (fst)
import Prelude (Show (show), ($!), (+), (-), putStr, putStrLn)

type BMCTree = Free BMCTreeF

data BMCTreeF next
  = CreateSBool String next
  | CreateSInteger String next
  | GetSBool String (SBool -> next)
  | GetSInteger String (SInteger -> next)
  | GetDepth (Maybe Int -> next)
  | Extend next
  | InitialConstraint next
  | AddProperty next
  | RemoveProperty next
  | AddTransition next
  | RemoveTransition next
  | ImplySuccessorState next
  | CheckSatisfiability (Bool -> next)
  | Log String next
  deriving (Functor)

createSBool :: String -> BMCTree ()
createSBool name = liftF $ CreateSBool name ()

createSInteger :: String -> BMCTree ()
createSInteger name = liftF $ CreateSInteger name ()

initialConstraint :: BMCTree ()
initialConstraint = liftF $ InitialConstraint ()

getSBool :: String -> BMCTree SBool
getSBool name = Free $ GetSBool name Pure

getSInteger :: String -> BMCTree SInteger
getSInteger name = Free $ GetSInteger name Pure

extend :: BMCTree ()
extend = liftF $ Extend ()

addTransition :: BMCTree ()
addTransition = liftF $ AddTransition ()

addProperty :: BMCTree ()
addProperty = liftF $ AddProperty ()

removeTransition :: BMCTree ()
removeTransition = liftF $ RemoveTransition ()

removeProperty :: BMCTree ()
removeProperty = liftF $ RemoveProperty ()

implySuccessorState :: BMCTree ()
implySuccessorState = liftF $ ImplySuccessorState ()

checkSatisfiability :: BMCTree Bool
checkSatisfiability = Free $ CheckSatisfiability Pure

log :: String -> BMCTree ()
log message = liftF $ Log message ()

logLn :: String -> BMCTree ()
logLn = log . ($!) (flip (++)) "\n"

bmc :: Maybe Int -> BMCTree Bool
bmc depth = do
  log "Level 0..."
  initialConstraint
  addProperty
  isSat <- checkSatisfiability
  if isSat
    then do
      removeProperty
      bmc' depth 0
    else do
      logLn "UNSAT"
      return False
  where
    bmc' (Just d) l
      | d <= l = return True
    bmc' _ level =
      do
        log $! "Level " ++ show level ++ "..."
        extend
        implySuccessorState
        addTransition
        addProperty
        isSat <- checkSatisfiability
        if isSat
          then do
            logLn "SAT"
            removeTransition
            removeProperty
            bmc' depth (level + 1)
          else do
            logLn "UNSAT"
            return False

newState :: VariableState -> Symbolic VariableState
newState VariableState {boolVars, intVars} =
  do
    newBools <- sBools boolNames
    newInts <- sIntegers intNames
    return (VariableState (zip boolNames newBools) (zip intNames newInts))
  where
    boolNames = map fst boolVars
    intNames = map fst intVars

bmcTreeToSbv :: VariableState -> VariableState -> BMCTree a -> Symbolic ()
bmcTreeToSbv _ nextState (Free (Extend next)) = newState nextState >>= flip (bmcTreeToSbv nextState) next
bmcTreeToSbv lastState nextState (Free (ImplySuccessorState next)) = constrain (nextState .== lastState) >> bmcTreeToSbv lastState nextState next
bmcTreeToSbv lastState nextState (Free (Log msg next)) = (return . putStr) msg >> bmcTreeToSbv lastState nextState next
bmcTreeToSbv _ _ _ = (return . putStrLn) "Not implemented yet" >> return ()