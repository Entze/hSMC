{-# LANGUAGE FlexibleContexts #-}
module Program where

import Data.SBV
import Data.List
import Data.Maybe
import Data.SExpresso.SExpr
import Data.SExpresso.Parse
import qualified Data.Text as Text

data ProgramType = ProgramInt | ProgramBool deriving (Eq, Show)
data LogicOp = EquivOP | XorOP | ImpliesOP | AndOP deriving (Eq, Show)
data CompOp = LEqualOP | GEqualOP | LessOP | GreaterOP | UnequalOP | EqualOP deriving (Eq, Show)
data ArithOp = PlusOP deriving (Eq, Show)



data Implication = Implies [Clause] [Clause] deriving (Show, Eq)
data Clause = LogicClause LogicOp Clause Clause | CompClause CompOp Term Term | BoolVar String | BoolConst String deriving (Show, Eq)
data Term = Term ArithOp Term Term | IntVar String | IntConst String deriving (Show, Eq)

data Program = Program ProgramState ProgramInit ProgramTransition ProgramProperty deriving (Eq, Show)
data ProgramState = State [(String, ProgramType)] deriving (Eq, Show)
data ProgramInit = Init [Clause] deriving (Eq, Show)
data ProgramTransition = Transition [Implication] deriving (Eq, Show)
data ProgramProperty = Property [Implication] deriving (Eq, Show)

data VariableState = VariableState {
    boolVarNames :: [String],
    boolSVars :: [SBool],
    intVarNames :: [String],
    intSVars :: [SInteger]
    } deriving (Eq, Show)


lookupBoolVar :: String -> VariableState -> SBool
lookupBoolVar str varState = maybe (literal False) ((boolSVars varState) !!) ix
    where
        ix = str `elemIndex` (boolVarNames varState)

lookupIntVar :: String -> VariableState -> SInteger
lookupIntVar str varState = maybe (literal 0) ((intSVars varState) !!) ix
    where
        ix = str `elemIndex` (intVarNames varState)


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

translateProgram :: Int -> Program -> Symbolic SBool
translateProgram n (Program
    (State state)
    (Init init)
    (Transition transition)
    (Property property)
    )
    | n <= 0 = do
        sIntVars <- sIntegers intVars
        shadowIntVars <- sIntegers shadowIntVars
        -- interleave or concat sIntVars & shadowIntVars
--        (sequence_ . map (constrain . (translateInitExpr intVars sIntVars))) init
        return (literal False)
--        (return . sAnd . map (translatePropertyExpr intVars sIntVars)) property
--  | otherwise = return ()
    where
        --TODO: boolVars, arrayVars, etc.
        intVars = map fst intVars'
        shadowIntVars = map (++ "_") intVars
        intVars' = filter ((== ProgramInt) . snd) state



translateBoolClause :: VariableState -> Clause -> Symbolic SBool
translateBoolClause varIndex (BoolVar var) = return (var `lookupBoolVar` varIndex)
translateBoolClause _ (BoolConst const) = (return . literal . (read :: String -> Bool)) const
translateBoolClause varIndex (CompClause op term1 term2) = do
                                                        term1' <- (translateIntTerm varIndex) term1
                                                        term2' <-  (translateIntTerm varIndex) term2
                                                        return ((translateCompOp op) term1' term2')
translateBoolClause varIndex (LogicClause op clause1 clause2) = do 
                                                        clause1' <- (translateBoolClause varIndex) clause1
                                                        clause2' <- (translateBoolClause varIndex) clause2
                                                        return ((translateLogicOp op) clause1' clause2')

translateIntTerm :: VariableState -> Term -> Symbolic SInteger
translateIntTerm varIndex (IntVar var) =  return (var `lookupIntVar` varIndex)
translateIntTerm _ (IntConst const) = (return . literal . (read :: String -> Integer)) const
translateIntTerm varIndex (Term op term1 term2) = do
                                                        term1' <- (translateIntTerm varIndex) term1
                                                        term2' <-  (translateIntTerm varIndex) term2
                                                        return ((translateArithOp op) term1' term2')