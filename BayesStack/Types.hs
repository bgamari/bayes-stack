{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Types ( Probability
                        , HasLikelihood(..)
                        ) where

import GHC.Prim (Constraint)
import Numeric.Log

type Probability = Log Double

class HasLikelihood p where
  type LContext p a :: Constraint
  type LContext p a = ()
  likelihood :: LContext p a => p a -> Probability
