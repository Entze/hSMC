{-# LANGUAGE FlexibleContexts #-}
module Program where

import Data.SBV
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map.Strict as Map
import Safe

data ProgramType = ProgramInt | ProgramBool deriving (Eq, Show)
data LogicOp = EquivOP | XorOP | ImpliesOP | AndOP deriving (Eq, Show)
data CompOp = LEqualOP | GEqualOP | LessOP | GreaterOP | UnequalOP | EqualOP deriving (Eq, Show)
data ArithOp = PlusOP deriving (Eq, Show)



data Implication = Implies [Clause] [Clause] deriving (Show, Eq)
data Clause = LogicClause LogicOp Clause Clause | CompClause CompOp Term Term | BoolVar String | BoolConst String deriving (Show, Eq)
data Term = Term ArithOp Term Term | IntVar String | IntConst String deriving (Show, Eq)

data Program = Program ProgramState ProgramInit ProgramTransition ProgramProperty deriving (Eq, Show)
newtype ProgramState = State [(String, ProgramType)] deriving (Eq, Show)
newtype ProgramInit = Init [Clause] deriving (Eq, Show)
newtype ProgramTransition = Transition [Implication] deriving (Eq, Show)
newtype ProgramProperty = Property [Implication] deriving (Eq, Show)

data VariableState = VariableState {
  boolVars :: Map.Map String (SBool, SBool),
  intVars :: Map.Map String (SInteger, SInteger)
}


lookupBoolVar :: String -> VariableState -> SBool
lookupBoolVar str varState
  | last str == '_' && Map.notMember deshadowed index = (snd . find) deshadowed
  | otherwise = (fst . find) str
    where
      index = boolVars varState
      deshadowed = init str
      find = flip (Map.findWithDefault (literal False, literal False)) index

lookupIntVar :: String -> VariableState -> SInteger
lookupIntVar str varState
  | last str == '_' && Map.notMember deshadowed index = (snd . find) deshadowed
  | otherwise = (fst . find) str
    where
      index = intVars varState
      deshadowed = init str
      find = flip (Map.findWithDefault (literal 0, literal 0)) index


translateArithOp :: ArithOp -> (SInteger -> SInteger -> SInteger)
translateArithOp PlusOP = (+)

translateLogicOp :: LogicOp -> (SBool -> SBool -> SBool)
translateLogicOp EquivOP = (.==)
translateLogicOp XorOP = (./=)
translateLogicOp ImpliesOP = (.=>)
translateLogicOp AndOP = (.&&)

translateCompOp :: CompOp -> (SInteger -> SInteger -> SBool)
translateCompOp EqualOP = (.==)
translateCompOp UnequalOP = (./=)
translateCompOp GEqualOP = (.>=)
translateCompOp LessOP = (.<)
translateCompOp LEqualOP = (.<=)
translateCompOp GreaterOP = (.>)

bmc :: Int -> Program -> IO ()
bmc bound program = bmc' 0 []
  where
  bmc' n oldstates
    | n >= bound = putStrLn "Bound exceeded."
    | n == 0 = do
      putStrLn ("Level " ++ show n)
      let newState = buildState program
      let toProve = do {
        ns <- newState;
        translateProgram Nothing ns program
      }
      isSat <- isSatisfiable toProve
      if isSat then
        (do {
        putStrLn "Counterexample found:";
        result <- prove toProve;
        print result;
      })
        else bmc' (n+1) (newState:oldstates)
    | otherwise = do
      putStrLn ("Level " ++ show n)
      let newState = buildState program
      let toProve = do {
        os <- head oldstates;
        ns <- newState;
        translateProgram (Just os) ns program
      }
      isSat <- isSatisfiable toProve
      if isSat then
        (do {
        putStrLn "Counterexample found:";
        result <- prove toProve;
        print result;
      })
        else bmc' (n+1) (newState:oldstates)

translateProgram :: Maybe VariableState -> VariableState -> Program -> Symbolic SBool
translateProgram oldState newState prog@(Program (State state)
              (Init init)
              (Transition transition)
              (Property property))
  = do
    mapM_ (constrain . translateBoolClause newState) init
    (return . sAll (translateImplies newState)) property

buildState :: Program -> Symbolic VariableState
buildState (Program (State state) _ _ _) 
  = do
    sIntVars <- sIntegers intVars'
    shadowSIntVars <- sIntegers intVars'
    sBoolVars <- sBools boolVars'
    shadowSBoolVars <- sBools boolVars'
    return VariableState { boolVars = Map.fromList (zip boolVars' (zip sBoolVars shadowSBoolVars)),
                      intVars = Map.fromList (zip intVars' (zip sIntVars shadowSIntVars))}
  where
    intVars' = (map fst . filter ((== ProgramInt) . snd)) state
    boolVars' = (map fst . filter ((== ProgramBool) . snd)) state

connectStates :: VariableState -> VariableState -> [SBool]
connectStates oldState newState = connectStates' oldBoolVars newBoolVars ++ connectStates' oldIntVars newIntVars
  where
    connectStates' :: EqSymbolic a => Map.Map String (b,a) -> Map.Map String (a,c) -> [SBool]
    connectStates' old new = Map.elems (Map.mapWithKey (connectState new) old)
    connectState :: EqSymbolic a => Map.Map String (a,b) -> String -> (c, a) -> SBool
    connectState newState varName (_, shadow) = shadow .== fst (newState Map.! varName)
    oldBoolVars = boolVars oldState
    oldIntVars = intVars oldState
    newBoolVars = boolVars newState
    newIntVars = intVars newState

translateImplies :: VariableState -> Implication -> SBool
translateImplies varIndex (Implies clauses1 clauses2) = translateBoolClauses varIndex clauses1 .=> translateBoolClauses varIndex clauses2

translateBoolClauses :: VariableState -> [Clause] -> SBool
translateBoolClauses varIndex = sAnd . map (translateBoolClause varIndex)


translateBoolClause :: VariableState -> Clause -> SBool
translateBoolClause varIndex (BoolVar var) = var `lookupBoolVar` varIndex
translateBoolClause _ (BoolConst const) = (literal . (read :: String -> Bool)) const
translateBoolClause varIndex (CompClause op term1 term2)
  = translateCompOp op term1' term2'
    where
      term1' = translateIntTerm varIndex term1
      term2' = translateIntTerm varIndex term2

translateBoolClause varIndex (LogicClause op clause1 clause2)
  = translateLogicOp op clause1' clause2'
    where
      clause1' = translateBoolClause varIndex clause1
      clause2' = translateBoolClause varIndex clause2

translateIntTerm :: VariableState -> Term -> SInteger
translateIntTerm varIndex (IntVar var) = var `lookupIntVar` varIndex
translateIntTerm _ (IntConst const) = (literal . (read :: String -> Integer)) const
translateIntTerm varIndex (Term op term1 term2)
  = translateArithOp op term1' term2'
    where
      term1' = translateIntTerm varIndex term1
      term2' = translateIntTerm varIndex term2
