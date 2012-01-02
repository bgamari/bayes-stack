{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Core( GatedPlate
                      , BayesStack.Core.sample
                      , Probability
                      , ProbDist(..)
                      , Sampleable(..)
                      , module BayesStack.Core.Shared
                      , module BayesStack.Core.ModelMonad
                      ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
  
import Data.Random hiding (Sampleable)
import qualified Data.Random.Distribution.Categorical as C

import Control.Monad
import GHC.Prim (Constraint)

import BayesStack.Core.ModelMonad
import BayesStack.Core.Shared
  
type GatedPlate control dist = EnumMap control (Shared dist)

sample :: Sampleable unit => unit -> ModelMonad ()
sample unit = 
  do unset unit
     r <- range unit
     probs <- forM r $ sampleProb unit
     let dist = C.fromWeightedList $ zip probs r
     new <- Data.Random.sample dist
     set unit new
    
type Probability = Double

class Sampleable unit where
  type SValue unit :: *
  unset :: unit -> ModelMonad ()
  range :: unit -> ModelMonad [SValue unit]
  sampleProb :: unit -> SValue unit -> ModelMonad Probability
  set :: unit -> SValue unit -> ModelMonad ()

class ProbDist p where
  type PdContext p a :: Constraint
  type PdContext p a = ()
  prob :: PdContext p a => p a -> a -> Probability

