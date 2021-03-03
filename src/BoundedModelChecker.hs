{-# LANGUAGE DeriveFunctor #-}

module BoundedModelChecker where

import CommonTypes (Type (..))
import Control.Monad.Free (Free (..))
import Data.Bool (Bool)
import Data.Functor (Functor)
import Data.SBV (SBV (..), SBool (..), SInteger (..))
import Data.String (String)
import Prelude ()

type BMCTree = Free BMCTreeF

data BMCTreeF next
  = CreateSBool String next
  | CreateSInteger String next
  | Constrain SBool next
  | GetSBool String (SBool -> next)
  | GetSInteger String (SInteger -> next)
  | Extend next
  | ImplySuccessorState next
  | CheckSatisfiability (Bool -> next)
  deriving (Functor)
