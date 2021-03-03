{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module ProgramInterpreter where

import CommonTypes (Type (..), Variable (..))
import Control.Monad (Functor, forM_)
import Control.Monad.Free as FreeMonad (Free (Free, Pure), liftF)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Function (id, ($))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List (length, (++))
import Data.Ord (Ord)
import Data.String (String)
import Prelude (Enum, Integer, Show (show), ($!), (-))

type Program = Free ProgramF

toSmtLibString Bool = "Bool"
toSmtLibString Integer = "Int"

data Connective = And | Or | Implies | Equals | LessEq | Greater | Plus deriving (Show, Enum, Eq, Ord)

data EvaluationStackElement = Connect Connective Int | VariableElement Variable | BoolConstant Bool | IntegerConstant Integer deriving (Show, Eq)

connective :: Connective -> Int -> EvaluationStackElement
connective = Connect

var = VariableElement

boolConst = BoolConstant

integerConst = IntegerConstant

data ProgramF next
  = Program next
  | State next
  | Init next
  | Transition next
  | Property next
  | VariableDeclaration String Type next
  | Push EvaluationStackElement next
  deriving (Functor, Show, Eq)

program :: Program ()
program = liftF $ Program ()

state :: Program ()
state = liftF $ State ()

init :: Program ()
init = liftF $ Init ()

transition :: Program ()
transition = liftF $ Transition ()

property :: Program ()
property = liftF $ Property ()

varDecl :: String -> Type -> Program ()
varDecl name varType = liftF $ VariableDeclaration name varType ()

push :: EvaluationStackElement -> Program ()
push elem = liftF $ Push elem ()

connect :: Connective -> [EvaluationStackElement] -> Program ()
connect connective operands =
  do
    push (Connect connective arity)
    forM_ operands push
  where
    arity = length operands

equals :: EvaluationStackElement -> EvaluationStackElement -> Program ()
equals a b = connect Equals [a, b]

implies :: EvaluationStackElement -> EvaluationStackElement -> Program ()
implies a b = connect Implies [a, b]

and :: [EvaluationStackElement] -> Program ()
and = connect And

plus :: EvaluationStackElement -> EvaluationStackElement -> Program ()
plus a b = connect Plus [a, b]

lessEq :: EvaluationStackElement -> EvaluationStackElement -> Program ()
lessEq a b = connect LessEq [a, b]

greater :: EvaluationStackElement -> EvaluationStackElement -> Program ()
greater a b = connect Greater [a, b]

printProgramTree :: Program a -> String
printProgramTree (Free (Program next@(Free State {}))) = ($!) (++) "(program \n" (printProgramTree next)
printProgramTree (Free (State next@(Free VariableDeclaration {}))) = ($!) (++) "  (state\n" (printProgramTree next)
printProgramTree (Free (VariableDeclaration name varType next@(Free VariableDeclaration {}))) = ($!) (++) ("    (" ++ name ++ " " ++ toSmtLibString varType ++ ")\n") (printProgramTree next)
printProgramTree (Free (VariableDeclaration name varType next@(Free Init {}))) = ($!) (++) ("    (" ++ name ++ " " ++ toSmtLibString varType ++ "))\n") (printProgramTree next)
printProgramTree (Free (Init next@(Free Transition {}))) = ($!) (++) "  (init)\n" (printProgramTree next)
printProgramTree (Free (Init next@(Free Push {}))) = ($!) (++) "  (init\n" (printProgramTree next)
printProgramTree (Free (Push (Connect Equals n) next)) = ($!) (++) "    (=" (printProgramTreeInContext next [n])
printProgramTree (Free (Push (Connect Implies n) next)) = ($!) (++) "    (=>" (printProgramTreeInContext next [n])
printProgramTree (Free (Transition next@(Free Push {}))) = ($!) (++) ")\n  (transition\n" (printProgramTree next)
printProgramTree (Free (Property next@(Free Push {}))) = ($!) (++) ")\n  (property\n" (printProgramTree next)
printProgramTree (Pure _) = "))"
printProgramTree _ = "\nError while parsing"

printProgramTreeInContext :: Program a -> [Int] -> String
printProgramTreeInContext next [] = printProgramTree next
printProgramTreeInContext next (0 : ns) = ')' : '\n' : ' ' : ' ' : printProgramTreeInContext next ns
printProgramTreeInContext (Free (Push (VariableElement (Var name _)) next)) (n : ns) = ($!) (++) (' ' : name) (printProgramTreeInContext next (n -1 : ns))
printProgramTreeInContext (Free (Push (BoolConstant b) next)) (n : ns) = ($!) (++) (' ' : show b) (printProgramTreeInContext next (n -1 : ns))
printProgramTreeInContext (Free (Push (IntegerConstant i) next)) (n : ns) = ($!) (++) (' ' : show i) (printProgramTreeInContext next (n -1 : ns))
printProgramTreeInContext (Free (Push (Connect Equals inner) next)) (n : ns) = ($!) (++) " (=" (printProgramTreeInContext next (inner : n -1 : ns))
printProgramTreeInContext (Free (Push (Connect Implies inner) next)) (n : ns) = ($!) (++) " (=>" (printProgramTreeInContext next (inner : n -1 : ns))
printProgramTreeInContext (Free (Push (Connect And inner) next)) (n : ns) = ($!) (++) " (and" (printProgramTreeInContext next (inner : n -1 : ns))
printProgramTreeInContext (Free (Push (Connect LessEq inner) next)) (n : ns) = ($!) (++) " (<=" (printProgramTreeInContext next (inner : n -1 : ns))
printProgramTreeInContext (Free (Push (Connect Greater inner) next)) (n : ns) = ($!) (++) " (>" (printProgramTreeInContext next (inner : n -1 : ns))
printProgramTreeInContext (Free (Push (Connect Plus inner) next)) (n : ns) = ($!) (++) " (+" (printProgramTreeInContext next (inner : n -1 : ns))
printProgramTreeInContext _ _ = "\nError while parsing in context"