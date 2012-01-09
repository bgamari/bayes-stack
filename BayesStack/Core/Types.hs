{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Core.Types ( SharedEnumMap, newSharedEnumMap
                             , Probability
                             , ProbDist(..)
                             , PretendableProbDist(..)
                             ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import GHC.Prim (Constraint)

import Control.Monad

import BayesStack.Core.ModelMonad
import BayesStack.Core.Shared

type SharedEnumMap control dist = EnumMap control (Shared dist)

newSharedEnumMap :: Enum control => [control] -> (control -> ModelMonad dist) -> ModelMonad (SharedEnumMap control dist)
newSharedEnumMap domain f =
  liftM EM.fromList $ forM domain $ \c -> do d <- f c >>= newShared
                                             return (c, d)

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

