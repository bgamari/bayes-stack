{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Core.Types ( Probability
                             , ProbDist(..)
                             , PretendableProbDist(..)
                             ) where

import GHC.Prim (Constraint)

import BayesStack.Core.ModelMonad
import BayesStack.Core.Shared

type Probability = Double

-- | A map over a domain 'a' to probabilities
class ProbDist p where
  type PdContext p a :: Constraint
  type PdContext p a = ()
  -- | 'prob p a' is the probability of 'a' in 'p'
  prob :: PdContext p a => p a -> a -> Probability
 
class PretendableProbDist p where
  type PpdContext p a :: Constraint
  type PpdContext p a = ()
  -- | 'probPretend p a' is the probability of 'a' in 'p' pretending that the 
  probPretend :: PdContext p a => p a -> a -> Probability

