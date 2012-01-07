{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Core.Types ( GatedPlate, newGatedPlate, forPlate
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

type GatedPlate control dist = EnumMap control (Shared dist)

newGatedPlate :: Enum control => [control] -> (control -> ModelMonad dist) -> ModelMonad (GatedPlate control dist)
newGatedPlate domain f =
  liftM EM.fromList $ forM domain $ \c -> do d <- f c >>= newShared
                                             return (c, d)

forPlate :: Enum control => GatedPlate control dist -> (control -> dist -> ModelMonad a) -> ModelMonad [(control,a)]
forPlate gp m = forM (EM.toList gp) $ \(c,d) -> do d' <- getShared d
                                                   r <- m c d'
                                                   return (c,r)

-- TODO: think through this
--plateIndex :: GatedPlate control dist -> control -> ModelMonad dist
--plateIndex a b = getShared . plateIndex a b
--plateIndex' :: GatedPlate control dist -> control -> ModelMonad (Shared dist)
--plateIndex' = (EM.!)

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

