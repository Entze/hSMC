{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.SBV
import ProgramParser
import ProgramInterpreter
import BoundedModelChecker as BMC
import CommonTypes
import Control.Monad.Free

main :: IO ()
main = putStrLn "Test suite not yet implemented"

sumString = "(program\n (state (pc Int)\n  (n Int)\n  (i Int)\n  (r Int))\n (init (= pc 0))\n (transition (=> (= pc 0) (and (= r_ 0)\n             (= i_ 0)\n             (= n_ n)\n             (= pc_ 1)))\n       (=> (and (= pc 1)\n          (<= i n)) (and (= r_ r)\n             (= i_ i)\n             (= n_ n)\n             (= pc_ 2)))\n       (=> (and (= pc 1)\n          (> i n)) (and (= r_ r)\n            (= n_ n)\n            (= pc_ 4)))\n       (=> (= pc 2) (and (= r_ (+ r i))\n             (= i_ i)\n             (= n_ n)\n             (= pc_ 3)))\n       (=> (= pc 3) (and (= r_ r)\n             (= i_ (+ i 1))\n             (= n_ n)\n             (= pc_ 1)\n             ))\n       (=> (= pc 4) (= pc_ 5))\n       (=> (= pc 5) (= pc_ 5)))\n (property (=> (= pc 4)\n         (= r 0))))\n"

sumExample = ProgramParser.Program
            (ProgramParser.State [("pc", ProgramInt),
                    ("n", ProgramInt),
                    ("i", ProgramInt),
                    ("r", ProgramInt)])
            (ProgramParser.Init [CompClause EqualOP (IntVar "pc") (IntConst "0")])
            (ProgramParser.Transition [ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "0")]
                          [CompClause EqualOP (IntVar "r_") (IntConst "0"),
                            CompClause EqualOP (IntVar "i_") (IntConst "0"),
                            CompClause EqualOP (IntVar "n_") (IntConst "n"),
                            CompClause EqualOP (IntVar "pc_") (IntConst "1")],
                        ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "1"),
                           CompClause LEqualOP (IntVar "i") (IntConst "n")]
                          [CompClause EqualOP (IntVar "r_") (IntVar "r"),
                            CompClause EqualOP (IntVar "i_") (IntVar "i"),
                            CompClause EqualOP (IntVar "n_") (IntVar "n"),
                            CompClause EqualOP (IntVar "pc_") (IntConst "2")],
                        ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "1"),
                            CompClause GreaterOP (IntVar "i") (IntVar "n")]
                          [CompClause EqualOP (IntVar "r_") (IntVar "r"),
                            CompClause EqualOP (IntVar "n_") (IntVar "n"),
                            CompClause EqualOP (IntVar "pc_") (IntConst "4")]
                          ,
                        ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "2")]
                          [CompClause EqualOP (IntVar "r_") (Term PlusOP (IntVar "r") (IntVar "i")),
                            CompClause EqualOP (IntVar "i_") (IntVar "i"),
                            CompClause EqualOP (IntVar "n_") (IntVar "n"),
                            CompClause EqualOP (IntVar "pc_") (IntConst "3")]
                          ,
                        ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "3")]
                          [CompClause EqualOP (IntVar "r_") (IntVar "r"),
                            CompClause EqualOP (IntVar "i_") (Term PlusOP (IntVar "i") (IntConst "1")),
                            CompClause EqualOP (IntVar "n_") (IntVar "n"),
                            CompClause EqualOP (IntVar "pc_") (IntConst "1")]
                          ,
                        ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "4")]
                          [CompClause EqualOP (IntVar "pc_") (IntConst "5")]
                          ,
                        ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "5")]
                          [CompClause EqualOP (IntVar "pc_") (IntConst "5")]
                          ])
            (ProgramParser.Property [ProgramParser.Implies
                          [CompClause EqualOP (IntVar "pc") (IntConst "4")]
                          [CompClause EqualOP (IntVar "r") (IntConst "0")]])

sumInit :: VariableState -> SBool
sumInit = flip translateBoolClauses init
  where
    (ProgramParser.Program _ (ProgramParser.Init init) _ _) = sumExample

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
    (ProgramParser.Program _ _ _ (ProgramParser.Property property)) = sumExample

--sumProgramTree :: ProgramInterpreter.Program
sumProgramTree = do
  program
  state
  varDecl "pc" Integer
  varDecl "n" Integer
  varDecl "i" Integer
  varDecl "r" Integer
  ProgramInterpreter.init
  equals pc (integerConst 0)
  ProgramInterpreter.transition
  -- pc = 0
  push (connective ProgramInterpreter.Implies 2)
  equals pc (integerConst 0)
  push (connective ProgramInterpreter.And 4)
  equals r_ (integerConst 0)
  equals i_ (integerConst 0)
  equals n_ n
  equals pc_ (integerConst 1)
  -- pc = 1, a
  push (connective ProgramInterpreter.Implies 2)
  push (connective ProgramInterpreter.And 2)
  equals pc (integerConst 1)
  lessEq i n
  push (connective ProgramInterpreter.And 4)
  equals r_ r
  equals i_ i
  equals n_ n
  equals pc_ (integerConst 2)
  -- pc = 1, b
  push (connective ProgramInterpreter.Implies 2)
  push (connective ProgramInterpreter.And 2)
  equals pc (integerConst 1)
  greater i n
  push (connective ProgramInterpreter.And 3)
  equals r_ r
  equals n_ n
  equals pc_ (integerConst 4)
  -- pc = 2
  push (connective ProgramInterpreter.Implies 2)
  equals pc (integerConst 2)
  push (connective ProgramInterpreter.And 4)
  push (connective ProgramInterpreter.Equals 2)
  push r_
  plus r i
  equals i_ i
  equals n_ n
  equals pc_ (integerConst 3)
  -- pc = 3
  push (connective ProgramInterpreter.Implies 2)
  equals pc (integerConst 3)
  push (connective ProgramInterpreter.And 4)
  equals r_ r
  push (connective ProgramInterpreter.Equals 2)
  push i_
  plus i (integerConst 1)
  equals n_ n
  equals pc_ (integerConst 1)
  -- pc = 4
  push (connective ProgramInterpreter.Implies 2)
  equals pc (integerConst 4)
  equals pc_ (integerConst 5)
  -- pc = 5
  push (connective ProgramInterpreter.Implies 2)
  equals pc (integerConst 5)
  equals pc_ (integerConst 5)
  ProgramInterpreter.property
  push (connective ProgramInterpreter.Implies 2)
  equals pc (integerConst 4)
  equals r (integerConst 0)
  where
    pc = var (Var "pc" Integer)
    pc_ = var (Var "pc_" Integer)
    n  = var (Var "n" Integer)
    n_ = var (Var "n_" Integer)
    i  = var (Var "i" Integer)
    i_ = var (Var "i_" Integer)
    r  = var (Var "r" Integer)
    r_ = var (Var "r_" Integer)


initialState = VariableState {
  boolVars = [],
  intVars = zip ["pc", "n", "i", "r"] (repeat (literal 0))
}

sumBMCTree :: BMC.BMCTree Bool
sumBMCTree
  = do
    BMC.log "Starting Proof"
    BMC.createSInteger "pc"
    BMC.createSInteger "n"
    BMC.createSInteger "i"
    BMC.createSInteger "r"
    BMC.bmc (Just 6)



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
