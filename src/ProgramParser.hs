{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgramParser where

import CommonTypes (VariableState (..), lookupBoolVar, lookupIntVar)
import Control.Monad as Monad (Functor, Monad (return), mapM_)
import Control.Monad.Free as FreeMonad ()
import Data.List as List (elemIndex, init)
import Data.Maybe as Maybe (Maybe, isNothing, maybe)
import Data.SBV as SBV
  ( EqSymbolic ((./=), (.==)),
    OrdSymbolic ((.<), (.<=), (.>), (.>=)),
    SBool,
    SInteger,
    SymVal (literal),
    Symbolic,
    constrain,
    sAll,
    sAnd,
    sFalse,
    (.&&),
    (.=>),
  )
import Data.SBV.Control as SBV.Control ()
import Safe ()
import Prelude

data ProgramType = ProgramInt | ProgramBool deriving (Eq, Show)

data LogicOp = EquivOP | XorOP | ImpliesOP | AndOP deriving (Eq, Show)

data CompOp = LEqualOP | GEqualOP | LessOP | GreaterOP | UnequalOP | EqualOP deriving (Eq, Show)

data ArithOp = PlusOP deriving (Eq, Show)

{-
interpret :: BMCTree a -> Symbolic a
interpret (EstablishVar store name ProgramInt next) = do
  var <- sInteger name
-}

--data ConstrainClause = EqualValue String Integer

data Implication = Implies [Clause] [Clause] deriving (Show, Eq)

data Clause = LogicClause LogicOp Clause Clause | CompClause CompOp Term Term | BoolVar String | BoolConst String deriving (Show, Eq)

data Term = Term ArithOp Term Term | IntVar String | IntConst String deriving (Show, Eq)

data Program = Program ProgramState ProgramInit ProgramTransition ProgramProperty deriving (Eq, Show)

newtype ProgramState = State [(String, ProgramType)] deriving (Eq, Show)

newtype ProgramInit = Init [Clause] deriving (Eq, Show)

newtype ProgramTransition = Transition [Implication] deriving (Eq, Show)

newtype ProgramProperty = Property [Implication] deriving (Eq, Show)

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

translateProgram :: Maybe VariableState -> VariableState -> Program -> Symbolic SBool
translateProgram
  oldState
  newState
  prog@( Program
           (State state)
           (Init init)
           (Transition transition)
           (Property property)
         ) =
    do
      mapM_ (constrain . translateBoolClause newState) init
      (return . sAll (translateImplies newState)) property

translateImplies :: VariableState -> Implication -> SBool
translateImplies varIndex (Implies clauses1 clauses2) = translateBoolClauses varIndex clauses1 .=> translateBoolClauses varIndex clauses2

translateBoolClauses :: VariableState -> [Clause] -> SBool
translateBoolClauses varIndex = sAnd . map (translateBoolClause varIndex)

translateBoolClause :: VariableState -> Clause -> SBool
translateBoolClause varIndex (BoolVar var) = var `lookupBoolVar` varIndex
translateBoolClause _ (BoolConst const) = (literal . (read :: String -> Bool)) const
translateBoolClause varIndex (CompClause op term1 term2) =
  translateCompOp op term1' term2'
  where
    term1' = translateIntTerm varIndex term1
    term2' = translateIntTerm varIndex term2
translateBoolClause varIndex (LogicClause op clause1 clause2) =
  translateLogicOp op clause1' clause2'
  where
    clause1' = translateBoolClause varIndex clause1
    clause2' = translateBoolClause varIndex clause2

translateIntTerm :: VariableState -> Term -> SInteger
translateIntTerm varIndex (IntVar var) = var `lookupIntVar` varIndex
translateIntTerm _ (IntConst const) = (literal . (read :: String -> Integer)) const
translateIntTerm varIndex (Term op term1 term2) =
  translateArithOp op term1' term2'
  where
    term1' = translateIntTerm varIndex term1
    term2' = translateIntTerm varIndex term2

{-
data SBVTree a where
  CreateIntVar :: String -> SBVTree SInteger
  CreateIntVars :: [String] -> SBVTree [SInteger]
  CreateBoolVar :: String -> SBVTree SBool
  CreateBoolVars :: [String] -> SBVTree [SBool]
  Connect :: EqSymbolic ta => (ta -> ta -> SBool) -> ta -> ta -> SBVTree SBool
  Combine :: SymVal ta => (ta -> ta -> ta) -> ta -> ta -> SBVTree ta
  Constrain :: SBool -> SBVTree ()
  Pure :: ta -> SBVTree ta
  Bind :: SBVTree ta -> (ta -> SBVTree tb) -> SBVTree tb

instance Functor SBVTree where
  fmap fn expr = expr >>= Pure . fn

instance Applicative SBVTree where
  pure = Pure
  fExpr <*> aExpr = fExpr >>= \f -> fmap f aExpr

instance Monad SBVTree where
  (>>=) = Bind

interpretAsSBV :: SBVTree a -> Symbolic a
interpretAsSBV (CreateIntVar name) = sInteger name
interpretAsSBV (CreateIntVars names) = sIntegers names
interpretAsSBV (CreateBoolVar name) = sBool name
interpretAsSBV (CreateBoolVars names) = sBools names
interpretAsSBV (Constrain pred) = constrain pred
interpretAsSBV (Pure p) = pure p
interpretAsSBV (Bind val next) = interpretAsSBV val >>= interpretAsSBV . next
-}
