{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.Multinomial ( -- * Dirichlet/multinomial pair
                                Multinom
                                -- * Construction
                              , fromPrior, fromProbs
                                -- * Querying
                              , total, prior, domain
                              , obsProb
                                -- * Adding and removing counts
                              , set, SetUnset (..)
                              , add, subtract
                              , increment, decrement
                                -- * Hyperparameter estimation
                              , estimatePrior, reestimatePriors, reestimateSymPriors
                              , updatePrior
                                -- * Convenience functions
                              , probabilities, decProbabilities
                              , prettyMultinom
                              ) where

import Prelude hiding (add, subtract)       
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES

import qualified Data.Foldable as Foldable
import Data.Foldable (toList, Foldable, foldMap)
import Data.Function (on)
import Data.List (sortBy)

import Text.PrettyPrint
import Text.Printf

import GHC.Generics (Generic)
import Data.Binary
import Data.Binary.EnumMap ()

import BayesStack.Types
import BayesStack.Dirichlet (Dirichlet, alphaOf)
import qualified BayesStack.Dirichlet as Dir

import Numeric.Log hiding (sum)
import Numeric.Digamma
import Math.Gamma hiding (p)

-- | Make error handling a bit easier
checkNaN :: RealFloat a => String -> a -> a
checkNaN loc x | isNaN x = error $ "BayesStack.Multinomial:"++loc++": Not a number"
checkNaN loc x | isInfinite x = error $ "BayesStack.Multinomial:"++loc++": Infinity"
checkNaN _ x = x

maybeInc, maybeDec :: (Num a, Eq a) => Maybe a -> Maybe a
maybeInc Nothing = Just 1
maybeInc (Just n) = Just (n+1)
maybeDec Nothing = error "Can't decrement zero count"
maybeDec (Just 1) = Nothing
maybeDec (Just n) = Just (n-1)

{-# INLINEABLE decrement #-}
{-# INLINEABLE increment #-}
decrement, increment :: (Num w, Eq w, Ord a, Enum a)
                         => a -> Multinom w a -> Multinom w a
decrement k = subtract 1 k
increment k = add 1 k

subtract, add :: (Num w, Eq w, Ord a, Enum a)
                         => w -> a -> Multinom w a -> Multinom w a
subtract w k dm = dm { counts = EM.alter maybeDec k $ counts dm
                        , total = total dm - w }
add w k dm = dm { counts = EM.alter maybeInc k $ counts dm
                        , total = total dm + w }

data SetUnset = Set | Unset

set :: (Num w, Eq w, Enum a, Ord a) => SetUnset -> a -> Multinom w a -> Multinom w a
set Set   s = increment s
set Unset s = decrement s

-- | 'Multinom a' represents multinomial distribution over domain 'a'.
-- Optionally, this can include a collapsed Dirichlet prior.
-- 'Multinom alpha count total' is a multinomial with Dirichlet prior
-- with symmetric parameter 'alpha', ...
data Multinom w a = DirMulti { prior  :: !(Dirichlet a)
                             , counts :: !(EnumMap a w)
                             , total  :: !w
                             }
                  | Multinom { probs  :: !(EnumMap a Double)
                             , counts :: !(EnumMap a w)
                             , total  :: !w
                             }
                  deriving (Show, Eq, Generic)
instance (Enum a, Binary a, Binary w) => Binary (Multinom w a)

-- | Construct a Dirichlet-multinomial distribution         
fromPrior :: (Num w, Enum a) => Dirichlet a -> Multinom w a
fromPrior a = DirMulti { prior = a
                       , counts = EM.empty
                       , total = 0
                       }
          
-- | A multinomial without a prior
fromProbs :: (Num w, Enum a) => [(a,Double)] -> Multinom w a
fromProbs probs = Multinom { probs = EM.fromList probs
                           , counts = EM.empty
                           , total = 0
                           }

-- | The domain          
domain :: Enum a => Multinom w a -> EnumSet a
domain a@(DirMulti {}) = Dir.domain $ prior a
domain a@(Multinom {}) = EM.keysSet $ probs a

data Acc w = Acc !w !Probability

-- | Probability of seeing a set of observations
obsProb :: (Enum a, Real w, Functor f, Foldable f)
        => Multinom w a -> f (a, w) -> Probability
obsProb (Multinom {probs=prob}) obs =
    Foldable.product $ fmap (\(k,w)->(realToFrac $ prob EM.! k)^^w) obs
  where (^^) :: Real w => Log Double -> w -> Log Double
        x ^^ y = Exp $ realToFrac y * ln x
obsProb (DirMulti {prior=alpha}) obs =
    let go (Acc w p) (k',w') = Acc (w+w') (p*p')
           where p' = Exp $ checkNaN "obsProb"
                      $ lnGamma (realToFrac w' + alpha `alphaOf` k')
    in case Foldable.foldl' go (Acc 0 1) obs of
         Acc w p -> p / Dir.normalizer alpha
                    / Exp (lnGamma $ realToFrac w + Dir.precision alpha)
{-# INLINE obsProb #-}

countsOf :: (Enum a, Num w) => a -> Multinom w a -> w
countsOf k dm =
  EM.findWithDefault 0 k (counts dm)

instance HasLikelihood (Multinom w) where
  type LContext (Multinom w) a = (Real w, Ord a, Enum a)
  likelihood dm = obsProb dm $ EM.assocs $ counts dm
  {-# INLINEABLE likelihood #-}

instance FullConditionable (Multinom w) where
  type FCContext (Multinom w) a = (Real w, Ord a, Enum a)
  sampleProb (Multinom {probs=prob}) k = prob EM.! k
  sampleProb dm@(DirMulti {prior=a}) k =
    let alpha = a `alphaOf` k
        n = realToFrac $ countsOf k dm
        s = realToFrac $ total dm
    in (n + alpha) / (s + Dir.precision a)
  {-# INLINEABLE sampleProb #-}

-- | Tabulated probability mass function
probabilities :: (Real w, Ord a, Enum a) => Multinom w a -> [(Double, a)]
probabilities dm = map (\a->(obsProb dm a, a)) $ ES.toList $ domain dm
{-# INLINEABLE probabilities #-}

-- | Probabilities sorted decreasingly
decProbabilities :: (Real w, Ord a, Enum a, Num w) => Multinom w a -> [(Double, a)]
decProbabilities = sortBy (flip (compare `on` fst)) . probabilities

prettyMultinom :: (Real w, Ord a, Enum a) => Int -> (a -> String) -> Multinom w a -> Doc
prettyMultinom _ _ (Multinom {})         = error "TODO: prettyMultinom"
prettyMultinom n showA dm@(DirMulti {})  =
  text "DirMulti" <+> parens (text "alpha=" <> Dir.prettyPrint showA (prior dm))
  $$ nest 5 (fsep $ punctuate comma
            $ map (\(p,a)->text (showA a) <> parens (text $ printf "%1.2e" p))
            $ take n $ Data.Foldable.toList $ decProbabilities dm)

-- | Update the prior of a Dirichlet/multinomial
updatePrior :: (Dirichlet a -> Dirichlet a) -> Multinom w a -> Multinom w a
updatePrior _ (Multinom {}) = error "TODO: updatePrior"
updatePrior f dm = dm {prior=f $ prior dm}

-- | Relative tolerance in precision for prior estimation
estimationTol = 1e-8

reestimatePriors :: (Foldable f, Functor f, Real w, Enum a)
                 => f (Multinom w a) -> f (Multinom w a)
reestimatePriors dms =
  let usableDms = filter (\dm->total dm > 5) $ toList dms
      alpha = case () of
                _ | length usableDms <= 3 -> id
                otherwise -> const $ estimatePrior estimationTol usableDms
  in fmap (updatePrior alpha) dms

reestimateSymPriors :: (Foldable f, Functor f, Real w, Enum a)
                    => f (Multinom w a) -> f (Multinom w a)
reestimateSymPriors dms =
  let usableDms = filter (\dm->total dm > 5) $ toList dms
      alpha = case () of
                _ | length usableDms <= 3 -> id
                otherwise -> const $ Dir.symmetrize
                             $ estimatePrior estimationTol usableDms
  in fmap (updatePrior alpha) dms

-- | Estimate the prior alpha from a set of Dirichlet/multinomials
estimatePrior' :: (Real w, Enum a) => [Multinom w a] -> Dirichlet a -> Dirichlet a
estimatePrior' dms alpha =
  let dom = ES.toList $ domain $ head dms
      go k = let num = sum $ map (\i->digamma (realToFrac (countsOf k i) + alphaOf alpha k)
                                       - digamma (alphaOf alpha k)
                                 )
                       $ filter (\i->countsOf k i > 0) dms
                 total i = realToFrac $ sum $ map (\k->countsOf k i) dom
                 sumAlpha = sum $ map (alphaOf alpha) dom
                 denom = sum $ map (\i->digamma (total i + sumAlpha) - digamma sumAlpha) dms
             in case () of
                  _ | isNaN num  -> error $ "BayesStack.DirMulti.estimatePrior': num = NaN: "++show (map (\i->(digamma (realToFrac (countsOf k i) + alphaOf alpha k), digamma (alphaOf alpha k))) dms)
                  _ | denom == 0 -> error "BayesStack.DirMulti.estimatePrior': denom=0"
                  _ | isInfinite num -> error "BayesStack.DirMulti.estimatePrior': num is infinity "
                  _ | isNaN (alphaOf alpha k * num / denom) -> error $ "NaN"++show (num, denom)
                  otherwise  -> alphaOf alpha k * num / denom
  in Dir.fromConcentrations $ map (\k->(k, go k)) dom

estimatePrior :: (Real w, Enum a) => Double -> [Multinom w a] -> Dirichlet a
estimatePrior tol dms = iter $ prior $ head dms
  where iter alpha = let alpha' = estimatePrior' dms alpha
                         prec  = Dir.precision alpha
                         prec' = Dir.precision alpha'
                     in if abs ((prec' - prec) / prec) > tol
                           then iter alpha'
                           else alpha'
