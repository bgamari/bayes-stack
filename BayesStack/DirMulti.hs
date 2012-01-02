{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             DirMulti, dirMulti
                           , probability
                           , probabilities
                           , decDirMulti, incDirMulti
                                          
                             -- * Categorical distributions
                           , CategoricalDist
                           , categoricalProbs, sortedCategoricalProbs
                           ) where

import qualified Data.Foldable 

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
  
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.List (sortBy)
import Data.Function (on)
 
import Data.Random.Distribution.Categorical

maybeInc, maybeDec :: Maybe Int -> Maybe Int
maybeInc Nothing = Just 1
maybeInc (Just n) = Just (n+1)
maybeDec Nothing = error "Can't decrement zero count"
maybeDec (Just 1) = Nothing
maybeDec (Just n) = Just (n-1)
                    
{-# INLINEABLE decDirMulti #-}
{-# INLINEABLE incDirMulti #-}
decDirMulti, incDirMulti :: (Ord a, Enum a) => a -> DirMulti a -> DirMulti a
decDirMulti k dm = dm { dmCounts = EM.alter maybeDec k $ dmCounts dm
                      , dmTotal = dmTotal dm - 1 }
incDirMulti k dm = dm { dmCounts = EM.alter maybeInc k $ dmCounts dm
                      , dmTotal = dmTotal dm + 1 }
                   
-- | 'DirMulti a' represents collapsed Dirichlet/multinomial pair over a domain 'a'.
-- 'DirMulti alpha count total' is a multinomial with Dirichlet prior
-- with symmetric parameter 'alpha', ...
data DirMulti a = DirMulti { dmAlpha :: Double 
                           , dmCounts :: EnumMap a Int
                           , dmTotal :: Int
                           , dmRange :: Seq a
                           }
                  deriving (Show, Eq)                           

dirMulti :: Double -> Seq a -> DirMulti a
dirMulti alpha range = DirMulti { dmAlpha = alpha
                                , dmCounts = EM.empty
                                , dmTotal = 0
                                , dmRange = range
                                }

{-# INLINEABLE probability #-}
instance ProbDist DirMulti where
  type ProbDistContext p a = (Ord a, Enum a)
  prob dm k = 
    
probability :: (Ord a, Enum a) => DirMulti a -> a -> Double
probability dm k =
  let c = realToFrac $ EM.findWithDefault 0 k counts
      DirMulti { dmAlpha=alpha, dmCounts=counts, dmTotal=total } = dm
      range = realToFrac $ SQ.length $ dmRange dm
  in (c + alpha + 1) / (realToFrac total + range * alpha + 1)

{-# INLINEABLE probabilities #-}
probabilities :: (Ord a, Enum a) => DirMulti a -> Seq (Double, a)
probabilities dm = fmap (\a->(probability dm a, a)) $ dmRange dm


-- FIXME Unnecessary constraints
class CategoricalDist d where
  categoricalProbs :: (Enum a, Ord a) => d a -> [(Double, a)]
  sortedCategoricalProbs :: (Enum a, Ord a) => d a -> [(Double, a)]
  sortedCategoricalProbs = sortBy (flip (compare `on` fst)) . categoricalProbs

instance CategoricalDist (Categorical Double) where
  categoricalProbs = toList

instance CategoricalDist DirMulti where
  categoricalProbs = Data.Foldable.toList . probabilities
    