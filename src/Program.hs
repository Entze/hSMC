{-# LANGUAGE FlexibleContexts, NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, GADTs #-}
module Program where

import Data.SBV
import Data.SBV.Control
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map.Strict as Map
import Safe

data ProgramType = ProgramInt | ProgramBool deriving (Eq, Show)
data LogicOp = EquivOP | XorOP | ImpliesOP | AndOP deriving (Eq, Show)
data CompOp = LEqualOP | GEqualOP | LessOP | GreaterOP | UnequalOP | EqualOP deriving (Eq, Show)
data ArithOp = PlusOP deriving (Eq, Show)

--data ConnectiveTree a where
--  Connective :: (SBool -> SBool -> SBool) -> SBool -> SBool -> ConnectiveTree SBool

data Implication = Implies [Clause] [Clause] deriving (Show, Eq)
data Clause = LogicClause LogicOp Clause Clause | CompClause CompOp Term Term | BoolVar String | BoolConst String deriving (Show, Eq)
data Term = Term ArithOp Term Term | IntVar String | IntConst String deriving (Show, Eq)

data Program = Program ProgramState ProgramInit ProgramTransition ProgramProperty deriving (Eq, Show)
newtype ProgramState = State [(String, ProgramType)] deriving (Eq, Show)
newtype ProgramInit = Init [Clause] deriving (Eq, Show)
newtype ProgramTransition = Transition [Implication] deriving (Eq, Show)
newtype ProgramProperty = Property [Implication] deriving (Eq, Show)

data VariableState = VariableState {
  boolVars :: [(String, SBool)],
  intVars :: [(String, SInteger)]
} deriving (Show, Eq)

instance EqSymbolic VariableState where
  VariableState {boolVars = boolVars1, intVars = intVars1} .== VariableState {boolVars = boolVars2, intVars = intVars2} = sAnd (zipWith (.==) (map snd boolVars1) (map snd boolVars2)) .&& sAnd (zipWith (.==) (map snd intVars1) (map snd intVars2))

--instance Fresh IO VariableState where
--  fresh = VariableState <$>

lookupBoolVar :: String -> VariableState -> SBool
lookupBoolVar str VariableState{boolVars, intVars}
  | last str == '_' && isNothing indexOf = maybe sFalse (vars !!) indexOf'
  | otherwise = maybe sFalse (vars !!) indexOf
    where
      (names,vars) = unzip boolVars
      indexOf = elemIndex str names
      indexOf' = elemIndex deshadowed names
      deshadowed = init str

lookupIntVar :: String -> VariableState -> SInteger
lookupIntVar str VariableState {boolVars, intVars}
  | last str == '_' && isNothing indexOf = maybe (literal 0) (vars !!) indexOf'
  | otherwise = maybe (literal 0) (vars !!) indexOf
    where
      (names,vars) = unzip intVars
      indexOf = elemIndex str names
      indexOf' = elemIndex deshadowed names
      deshadowed = init str


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

bmc :: (EqSymbolic st, Queriable IO st res)
        => SMTConfig
        -> Maybe Int
        -> Bool
        -> Symbolic ()
        -> (st -> SBool)
        -> (st -> [(SBool, st)])
        -> (st -> SBool)
        -> IO (Either String (Int, [res]))
bmc cfg mbLimit chatty setup initial trans goal = runSMTWith cfg $ do setup
                                                                      query $ do
                                                                              state <- create
                                                                              constrain $ initial state
                                                                              go 0 state []
   where go i _ _
          | Just l <- mbLimit, i >= l
          = return $ Left $ "BMC limit of " ++ show l ++ " reached"
         go i curState sofar = do when chatty $ io $ putStrLn $ "BMC: Iteration: " ++ show i
                                  push 1
                                  constrain $ goal curState
                                  cs <- checkSat
                                  case cs of
                                    DSat{} -> error "BMC: Solver returned an unexpected delta-sat result."
                                    Sat    -> do when chatty $ io $ putStrLn $ "BMC: Solution found at iteration " ++ show i
                                                 ms <- mapM project (curState : sofar)
                                                 return $ Right (i, reverse ms)
                                    Unk    -> do when chatty $ io $ putStrLn $ "BMC: Backend solver said unknown at iteration " ++ show  i
                                                 return $ Left $ "BMC: Solver said unknown in iteration " ++ show i
                                    Unsat  -> do pop 1
                                                 nextState <- create
                                                 let nextStates = trans curState
                                                 constrain $ sOr $ map (\ (pre, post) -> pre .&& nextState .== post) nextStates
                                                 go (i+1) nextState (curState : sofar)

translateProgram :: Maybe VariableState -> VariableState -> Program -> Symbolic SBool
translateProgram oldState newState prog@(Program (State state)
              (Init init)
              (Transition transition)
              (Property property))
  = do
    mapM_ (constrain . translateBoolClause newState) init
    (return . sAll (translateImplies newState)) property

{-
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
-}

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
