{-# LANGUAGE FlexibleContexts #-}
module Program where

import Data.SBV
import Data.List
import Data.Maybe

data ProgramType = ProgramInt | ProgramBool deriving (Eq, Show)
--data ProgramOperator = Equal | Implies | And | Plus deriving (Eq, Show)

{-
data ProgramAtom = 
    Const ProgramType String 
    | Var ProgramType String 
    | Operator ProgramOperator
    deriving (Eq, Show)
-}

data InitExpr =
    Equals String String
    | Unequal String String
    deriving (Eq, Show)

data ImplicationExpr = Implies [InitExpr] [InitExpr] deriving (Eq, Show)

data Expr = Null deriving (Eq, Show)

data Program = Program ProgramState ProgramInit ProgramTransition ProgramProperty deriving (Eq, Show)
data ProgramState = State [(String, ProgramType)] deriving (Eq, Show)
data ProgramInit = Init [InitExpr] deriving (Eq, Show)
data ProgramTransition = Transition [ImplicationExpr] deriving (Eq, Show)
data ProgramProperty = Property [ImplicationExpr] deriving (Eq, Show)

--data SymbolicTranslation = 

translateProgram :: Int -> Program -> SymbolicT IO SBool
translateProgram 0 (Program
    (State state)
    (Init init)
    (Transition transition)
    (Property property)
    ) = do
        sIntVars <- sIntegers intVars
        (sequence_ . map (constrain . (translateInitExpr intVars sIntVars))) init
        (return . sAnd . map (translatePropertyExpr intVars sIntVars)) property
    where
        --TODO: boolVars, arrayVars, etc.
        intVars = map fst intVars'
        intVars' = filter ((== ProgramInt) . snd) state

        
{-
translateExpr :: EqSymbolic a => Expr -> Either a SBool
translateExpr (Node (Operator Equal) list) = (Right . allEqual . (fromLeft) . map translateExpr) list
translateExpr (Node (Const ProgramInt val) []) = (Left . (read :: )) val
-}

translateInitExpr vars svars (Equals var val) = (svars !! (fromJust (var `elemIndex` vars))) .== (literal ((read val) :: Integer))

translateTranslationExpr vars svars (Implies pre post) = (sAnd (map (translateInitExpr vars svars) pre)) .=> (sAnd (map (translateInitExpr vars svars) post))

translatePropertyExpr vars svars (Implies pre post) = (sAnd (map (translateInitExpr vars svars) pre)) .=> (sAnd (map (translateInitExpr vars svars) post))