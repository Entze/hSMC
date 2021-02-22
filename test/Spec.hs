import Data.SBV
import qualified Data.Text as Text
import Program

main :: IO ()
main = putStrLn "Test suite not yet implemented"

sumString = "(program\n (state (pc Int)\n  (n Int)\n  (i Int)\n  (r Int))\n (init (= pc 0))\n (transition (=> (= pc 0) (and (= r_ 0)\n             (= i_ 0)\n             (= n_ n)\n             (= pc_ 1)))\n       (=> (and (= pc 1)\n          (<= i n)) (and (= r_ r)\n             (= i_ i)\n             (= n_ n)\n             (= pc_ 2)))\n       (=> (and (= pc 1)\n          (> i n)) (and (= r_ r)\n            (= n_ n)\n            (= pc_ 4)))\n       (=> (= pc 2) (and (= r_ (+ r i))\n             (= i_ i)\n             (= n_ n)\n             (= pc_ 3)))\n       (=> (= pc 3) (and (= r_ r)\n             (= i_ (+ i 1))\n             (= n_ n)\n             (= pc_ 1)\n             ))\n       (=> (= pc 4) (= pc_ 5))\n       (=> (= pc 5) (= pc_ 5)))\n (property (=> (= pc 4)\n         (= r 0))))\n"

{-
sumExample = (Program
            (State [("pc", ProgramInt),
                    ("n", ProgramInt),
                    ("i", ProgramInt),
                    ("r", ProgramInt)])
            (Init [(Equals "pc" "0")])
            (Transition [(Implies 
                          [(Equals "pc" "0")]
                          [(Equals "r_" "0"),
                            (Equals "i_" "0"),
                            (Equals "n_" "n"),
                            (Equals "pc_" "1")]),
                        (Implies
                          [(Equals "pc" "1"), 
                            (GEqual "i" "n")]
                          [(Equals "r_" "r"),
                            (Equals "i_" "i"),
                            (Equals "n_" "n"),
                            (Equals "pc_" "2")]),
                        (Implies 
                          [(Equals "pc" "1"),
                            (Less "i" "n")]
                          [(Equals "r_" "r"),
                            (Equals "n_" "n"),
                            (Equals "pc_" "4")]
                          ),
                        (Implies 
                          [(Equals "pc" "2")] 
                          [(Equals "r_" (Plus "r" "i")),
                            (Equals "i_" "i"),
                            (Equals "n_" "n"),
                            (Equals "pc_" "3")]
                          ),
                        (Implies 
                          [(Equals "pc" "3")]
                          [(Equals "r_" "r"),
                            (Equals "i_" (Plus "i" "1")),
                            (Equals "n_" "n"),
                            (Equals "pc_" "1")]
                          ,)
                        (Implies 
                          [(Equals "pc" "4")]
                          [(Equals "pc_" "5")]
                          ),
                        (Implies 
                          [(Equals "pc" "5")]
                          [(Equals "pc_" "5")]
                          )])
            (Property [(Implies 
                          [(Equals "pc" "4")]
                          [(Equals "r" "0")])]))
                          -}