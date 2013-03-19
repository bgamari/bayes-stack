{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Core.Types ( Probability
                             , HasLikelihood(..)
                             , FullConditionable(..)
                             ) where

import GHC.Prim (Constraint)
import Numeric.Log

type Probability = Log Double

class HasLikelihood p where
  type LContext p a :: Constraint
  type LContext p a = ()
  likelihood :: LContext p a => p a -> Probability

-- | A distribution for which a full conditional factor can be produced
class FullConditionable p where
  type FCContext p a :: Constraint
  type FCContext p a = ()
  sampleProb :: FCContext p a => p a -> a -> Double
