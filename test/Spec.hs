{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.SBV
import Program
import Control.Monad.Free

main :: IO ()
main = putStrLn "Test suite not yet implemented"

sumString = "(program\n (state (pc Int)\n  (n Int)\n  (i Int)\n  (r Int))\n (init (= pc 0))\n (transition (=> (= pc 0) (and (= r_ 0)\n             (= i_ 0)\n             (= n_ n)\n             (= pc_ 1)))\n       (=> (and (= pc 1)\n          (<= i n)) (and (= r_ r)\n             (= i_ i)\n             (= n_ n)\n             (= pc_ 2)))\n       (=> (and (= pc 1)\n          (> i n)) (and (= r_ r)\n            (= n_ n)\n            (= pc_ 4)))\n       (=> (= pc 2) (and (= r_ (+ r i))\n             (= i_ i)\n             (= n_ n)\n             (= pc_ 3)))\n       (=> (= pc 3) (and (= r_ r)\n             (= i_ (+ i 1))\n             (= n_ n)\n             (= pc_ 1)\n             ))\n       (=> (= pc 4) (= pc_ 5))\n       (=> (= pc 5) (= pc_ 5)))\n (property (=> (= pc 4)\n         (= r 0))))\n"

sumExample = (Program
            (State [("pc", ProgramInt),
                    ("n", ProgramInt),
                    ("i", ProgramInt),
                    ("r", ProgramInt)])
            (Init [(CompClause EqualOP (IntVar "pc") (IntConst "0"))])
            (Transition [(Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "0"))]
                          [(CompClause EqualOP (IntVar "r_") (IntConst "0")),
                            (CompClause EqualOP (IntVar "i_") (IntConst "0")),
                            (CompClause EqualOP (IntVar "n_") (IntConst "n")),
                            (CompClause EqualOP (IntVar "pc_") (IntConst "1"))]),
                        (Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "1")),
                            (CompClause LEqualOP (IntVar "i") (IntConst "n"))]
                          [(CompClause EqualOP (IntVar "r_") (IntVar "r")),
                            (CompClause EqualOP (IntVar "i_") (IntVar "i")),
                            (CompClause EqualOP (IntVar "n_") (IntVar "n")),
                            (CompClause EqualOP (IntVar "pc_") (IntConst "2"))]),
                        (Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "1")),
                            (CompClause GreaterOP (IntVar "i") (IntVar "n"))]
                          [(CompClause EqualOP (IntVar "r_") (IntVar "r")),
                            (CompClause EqualOP (IntVar "n_") (IntVar "n")),
                            (CompClause EqualOP (IntVar "pc_") (IntConst "4"))]
                          ),
                        (Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "2"))]
                          [(CompClause EqualOP (IntVar "r_") (Term PlusOP (IntVar "r") (IntVar "i"))),
                            (CompClause EqualOP (IntVar "i_") (IntVar "i")),
                            (CompClause EqualOP (IntVar "n_") (IntVar "n")),
                            (CompClause EqualOP (IntVar "pc_") (IntConst "3"))]
                          ),
                        (Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "3"))]
                          [(CompClause EqualOP (IntVar "r_") (IntVar "r")),
                            (CompClause EqualOP (IntVar "i_") (Term PlusOP (IntVar "i") (IntConst "1"))),
                            (CompClause EqualOP (IntVar "n_") (IntVar "n")),
                            (CompClause EqualOP (IntVar "pc_") (IntConst "1"))]
                          ),
                        (Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "4"))]
                          [(CompClause EqualOP (IntVar "pc_") (IntConst "5"))]
                          ),
                        (Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "5"))]
                          [(CompClause EqualOP (IntVar "pc_") (IntConst "5"))]
                          )])
            (Property [(Implies
                          [(CompClause EqualOP (IntVar "pc") (IntConst "4"))]
                          [(CompClause EqualOP (IntVar "r") (IntConst "0"))])]))

sumInit :: VariableState -> SBool
sumInit = flip translateBoolClauses init
  where
    (Program _ (Init init) _ _) = sumExample

sumTrans :: VariableState -> [(SBool, VariableState)]
sumTrans variableState = [(lookupIntVar "pc" variableState .== 0, VariableState {
  boolVars = [],
  intVars = [
    ("pc", literal 1),
    ("n", lookupIntVar "n" variableState),
    ("i", literal 0),
    ("r", literal 0)
  ]
  })]

sumGoal :: VariableState -> SBool
sumGoal variableState = sAll (translateImplies variableState) property
  where
    (Program _ _ _ (Property property)) = sumExample

{-
sumVerify :: SBVTree (Symbolic SBool) 
sumVerify = do
  x <- 
-}


{-
sumProgramTree :: Free ProgramTree Integer
sumProgramTree = do
  program (state (declare "pc" ProgramInt (declare "n" ProgramInt done))) (Program.init done) (transition done) (Program.property done)
  -}