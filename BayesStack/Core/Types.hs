{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Core.Types ( Probability
                             , ProbDist(..)
                             ) where

import GHC.Prim (Constraint)

type Probability = Double

-- | A map over a domain 'a' to probabilities
class ProbDist p where
  type PdContext p a :: Constraint
  type PdContext p a = ()
  -- | 'prob p a' is the probability of 'a' in 'p'
  prob :: PdContext p a => p a -> a -> Probability
 
