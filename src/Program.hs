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
data CompOp = GEqualOP | LessOP | UnequalOP | EqualOP deriving (Eq, Show)
data ArithOp = PlusOP deriving (Eq, Show)

{-
data ProgramAtom = 
    Const ProgramType String 
    | Var ProgramType String 
    | Operator ProgramOperator
    deriving (Eq, Show)
-}


--data Expr = String | Operator Expr Expr

--data Transplant = Operator String Transplant | Operator Translator String | Operator String String | Operator Transplant Transplant | And [Transplant]

data Implication = Implies [Clause] [Clause] deriving (Show, Eq)
data Clause = LogicClause LogicOp Clause Clause | ArithClause CompOp Term Term | BoolVar String | BoolConst String deriving (Show, Eq)
data Term = Term ArithOp Term Term | IntVar String | IntConst String deriving (Show, Eq)

--data ImplicationExpr = Implies [InitExpr] [InitImplication] deriving (Eq, Show)

data Program = Program ProgramState ProgramInit ProgramTransition ProgramProperty deriving (Eq, Show)
data ProgramState = State [(String, ProgramType)] deriving (Eq, Show)
data ProgramInit = Init [Clause] deriving (Eq, Show)
data ProgramTransition = Transition [Implication] deriving (Eq, Show)
data ProgramProperty = Property [Implication] deriving (Eq, Show)


translateProgram :: Int -> Program -> SymbolicT IO SBool
translateProgram n (Program
    (State state)
    (Init init)
    (Transition transition)
    (Property property)
    )
    | n <= 0 = do
        sIntVars <- sIntegers intVars
--        (sequence_ . map (constrain . (translateInitExpr intVars sIntVars))) init
        return (literal False)
--        (return . sAnd . map (translatePropertyExpr intVars sIntVars)) property
--  | otherwise = return ()
    where
        --TODO: boolVars, arrayVars, etc.
        intVars = map fst intVars'
        intVars' = filter ((== ProgramInt) . snd) state

        
{-
translateExpr :: EqSymbolic a => Expr -> Either a SBool
translateExpr (Node (Operator Equal) list) = (Right . allEqual . (fromLeft) . map translateExpr) list
translateExpr (Node (Const ProgramInt val) []) = (Left . (read :: )) val
-}

{-
translateInitExpr vars svars (Equals var val) = (svars !! (fromJust (var `elemIndex` vars))) .== (literal ((read val) :: Integer))

--translateTransitionExpr vars svars (Expr ImpliesOP pre post) = (sAnd (map (translateInitExpr vars svars) pre)) .=> (sAnd (map (translateInitExpr vars svars) post))

--translatePropertyExpr vars svars (Expr ImpliesOP pre post) = (sAnd (map (translateInitExpr vars svars) pre)) .=> (sAnd (map (translateInitExpr vars svars) post))

translateBoolClause :: Clause -> SBool
--translateBoolClause (Clause op (Const const1) (Const const2))
--translateBoolClause (Clause op (Const const1) clause2)
--translateBoolClause (Clause op clause1 (Const const2))
translateBoolClause (Clause op clause1 clause2)
    | op == EqualOP = (translateBoolClause clause1) .== (translateBoolClause clause2)
    | op == UnequalOP = (translateBoolClause clause1) ./= (translateBoolClause clause2)
    | op == ImpliesOP = (translateBoolClause clause1) .=> (translateBoolClause clause2)
    | op == AndOP = (translateBoolClause clause1) .&& (translateBoolClause clause2)
    | op == GEqualOP = (translateIntClause clause1) .>= (translateIntClause clause2)
    | op == LessOP = (translateIntClause clause1) .< (translateIntClause clause2)
    | op == PlusOP = (translateIntClause clause1) + (translateIntClause clause2)

translateIntClause :: Clause -> SInteger
--translateIntClause (Clause op (Const const1) (Const const2))
--translateIntClause (Clause op (Const const1) clause2)
--translateIntClause (Clause op clause1 (Const const2))
translateIntClause (Clause op clause1 clause2)
    | op == PlusOP = (translateIntClause clause1) + (translateIntClause clause2)

    -}