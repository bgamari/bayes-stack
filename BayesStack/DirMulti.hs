{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             Multinom, dirMulti, symDirMulti, multinom
                             -- | Do not do record updates with these
                           , dmTotal, dmAlpha, dmDomain
                           , setMultinom, SetUnset (..)
                           , addMultinom, subMultinom
                           , decMultinom, incMultinom
                           , prettyMultinom
                           , updatePrior
                           , obsProb
                             -- * Parameter estimation
                           , estimatePrior, reestimatePriors, reestimateSymPriors
                             -- * Convenience functions
                           , probabilities, decProbabilities
                           ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import qualified Data.Foldable as Foldable
import Data.Foldable (toList, Foldable, foldMap)
import Data.Function (on)

import Text.PrettyPrint
import Text.Printf

import GHC.Generics (Generic)
import Data.Binary
import Data.Binary.EnumMap ()

import BayesStack.Types
import BayesStack.Dirichlet

import Numeric.Log hiding (sum)
import Numeric.Digamma
import Math.Gamma hiding (p)

-- | Make error handling a bit easier
checkNaN :: RealFloat a => String -> a -> a
checkNaN loc x | isNaN x = error $ "BayesStack.DirMulti."++loc++": Not a number"
checkNaN loc x | isInfinite x = error $ "BayesStack.DirMulti."++loc++": Infinity"
checkNaN _ x = x

maybeInc, maybeDec :: (Num a, Eq a) => Maybe a -> Maybe a
maybeInc Nothing = Just 1
maybeInc (Just n) = Just (n+1)
maybeDec Nothing = error "Can't decrement zero count"
maybeDec (Just 1) = Nothing
maybeDec (Just n) = Just (n-1)

{-# INLINEABLE decMultinom #-}
{-# INLINEABLE incMultinom #-}
decMultinom, incMultinom :: (Num w, Eq w, Ord a, Enum a)
                         => a -> Multinom w a -> Multinom w a
decMultinom k = subMultinom 1 k
incMultinom k = addMultinom 1 k

subMultinom, addMultinom :: (Num w, Eq w, Ord a, Enum a)
                         => w -> a -> Multinom w a -> Multinom w a
subMultinom w k dm = dm { dmCounts = EM.alter maybeDec k $ dmCounts dm
                        , dmTotal = dmTotal dm - w }
addMultinom w k dm = dm { dmCounts = EM.alter maybeInc k $ dmCounts dm
                        , dmTotal = dmTotal dm + w }

data SetUnset = Set | Unset

setMultinom :: (Num w, Eq w, Enum a, Ord a) => SetUnset -> a -> Multinom w a -> Multinom w a
setMultinom Set   s = incMultinom s
setMultinom Unset s = decMultinom s

-- | 'Multinom a' represents multinomial distribution over domain 'a'.
-- Optionally, this can include a collapsed Dirichlet prior.
-- 'Multinom alpha count total' is a multinomial with Dirichlet prior
-- with symmetric parameter 'alpha', ...
data Multinom w a = DirMulti { dmAlpha :: !(Alpha a)
                             , dmCounts :: !(EnumMap a w)
                             , dmTotal :: !w
                             , dmDomain :: !(Seq a)
                             }
                  | Multinom { dmProbs :: !(EnumMap a Double)
                             , dmCounts :: !(EnumMap a w)
                             , dmTotal :: !w
                             , dmDomain :: !(Seq a)
                             }
                  deriving (Show, Eq, Generic)
instance (Enum a, Binary a, Binary w) => Binary (Multinom w a)

-- | 'symMultinomFromPrecision d p' is a symmetric Dirichlet/multinomial over a
-- domain 'd' with precision 'p'
symDirMultiFromPrecision :: (Num w, Enum a) => [a] -> DirPrecision -> Multinom w a
symDirMultiFromPrecision domain prec = symDirMulti (0.5*prec) domain

-- | 'dirMultiFromMeanPrecision m p' is an asymmetric Dirichlet/multinomial
-- over a domain 'd' with mean 'm' and precision 'p'
dirMultiFromPrecision :: (Num w, Enum a) => DirMean a -> DirPrecision -> Multinom w a
dirMultiFromPrecision m p = dirMultiFromAlpha $ meanPrecisionToAlpha m p

-- | Create a symmetric Dirichlet/multinomial
symDirMulti :: (Num w, Enum a) => Double -> [a] -> Multinom w a
symDirMulti alpha domain = dirMultiFromAlpha $ symAlpha domain alpha

-- | A multinomial without a prior
multinom :: (Num w, Enum a) => [(a,Double)] -> Multinom w a
multinom probs = Multinom { dmProbs = EM.fromList probs
                          , dmCounts = EM.empty
                          , dmTotal = 0
                          , dmDomain = SQ.fromList $ map fst probs
                          }

-- | Create an asymmetric Dirichlet/multinomial from items and alphas
dirMulti :: (Num w, Enum a) => [(a,Double)] -> Multinom w a
dirMulti domain = dirMultiFromAlpha $ asymAlpha $ EM.fromList domain

-- | Create a Dirichlet/multinomial with a given prior
dirMultiFromAlpha :: (Enum a, Num w) => Alpha a -> Multinom w a
dirMultiFromAlpha alpha = DirMulti { dmAlpha = alpha
                                   , dmCounts = EM.empty
                                   , dmTotal = 0
                                   , dmDomain = alphaDomain alpha
                                   }

data Acc w = Acc !w !Probability

obsProb :: (Enum a, Real w, Functor f, Foldable f)
        => Multinom w a -> f (a, w) -> Probability
obsProb (Multinom {dmProbs=prob}) obs =
    Foldable.product $ fmap (\(k,w)->(realToFrac $ prob EM.! k)^^w) obs
  where (^^) :: Real w => Log Double -> w -> Log Double
        x ^^ y = Exp $ realToFrac y * ln x
obsProb (DirMulti {dmAlpha=alpha}) obs =
    let go (Acc w p) (k',w') = Acc (w+w') (p*p')
           where p' = Exp $ checkNaN "obsProb"
                      $ lnGamma (realToFrac w' + alpha `alphaOf` k')
    in case Foldable.foldl' go (Acc 0 1) obs of
         Acc w p -> p / alphaNormalizer alpha
                    / Exp (lnGamma $ realToFrac w + sumAlpha alpha)
{-# INLINE obsProb #-}

dmGetCounts :: (Enum a, Num w) => Multinom w a -> a -> w
dmGetCounts dm k =
  EM.findWithDefault 0 k (dmCounts dm)

instance HasLikelihood (Multinom w) where
  type LContext (Multinom w) a = (Real w, Ord a, Enum a)
  likelihood dm = obsProb dm $ EM.assocs $ dmCounts dm
  {-# INLINEABLE likelihood #-}

instance FullConditionable (Multinom w) where
  type FCContext (Multinom w) a = (Real w, Ord a, Enum a)
  sampleProb (Multinom {dmProbs=prob}) k = prob EM.! k
  sampleProb dm@(DirMulti {dmAlpha=a}) k =
    let alpha = a `alphaOf` k
        n = realToFrac $ dmGetCounts dm k
        total = realToFrac $ dmTotal dm
    in (n + alpha) / (total + sumAlpha a)
  {-# INLINEABLE sampleProb #-}

{-# INLINEABLE probabilities #-}
probabilities :: (Real w, Ord a, Enum a) => Multinom w a -> Seq (Double, a)
probabilities dm = fmap (\a->(sampleProb dm a, a)) $ dmDomain dm -- FIXME

-- | Probabilities sorted decreasingly
decProbabilities :: (Real w, Ord a, Enum a, Num w) => Multinom w a -> Seq (Double, a)
decProbabilities = SQ.sortBy (flip (compare `on` fst)) . probabilities

prettyMultinom :: (Real w, Ord a, Enum a) => Int -> (a -> String) -> Multinom w a -> Doc
prettyMultinom _ _ (Multinom {})         = error "TODO: prettyMultinom"
prettyMultinom n showA dm@(DirMulti {})  =
  text "DirMulti" <+> parens (text "alpha=" <> prettyAlpha showA (dmAlpha dm))
  $$ nest 5 (fsep $ punctuate comma
            $ map (\(p,a)->text (showA a) <> parens (text $ printf "%1.2e" p))
            $ take n $ Data.Foldable.toList $ decProbabilities dm)

-- | Update the prior of a Dirichlet/multinomial
updatePrior :: (Alpha a -> Alpha a) -> Multinom w a -> Multinom w a
updatePrior _ (Multinom {}) = error "TODO: updatePrior"
updatePrior f dm = dm {dmAlpha=f $ dmAlpha dm}

-- | Relative tolerance in precision for prior estimation
estimationTol = 1e-8

reestimatePriors :: (Foldable f, Functor f, Real w, Enum a)
                 => f (Multinom w a) -> f (Multinom w a)
reestimatePriors dms =
  let usableDms = filter (\dm->dmTotal dm > 5) $ toList dms
      alpha = case () of
                _ | length usableDms <= 3 -> id
                otherwise -> const $ estimatePrior estimationTol usableDms
  in fmap (updatePrior alpha) dms

reestimateSymPriors :: (Foldable f, Functor f, Real w, Enum a)
                    => f (Multinom w a) -> f (Multinom w a)
reestimateSymPriors dms =
  let usableDms = filter (\dm->dmTotal dm > 5) $ toList dms
      alpha = case () of
                _ | length usableDms <= 3 -> id
                otherwise -> const $ symmetrizeAlpha $ estimatePrior estimationTol usableDms
  in fmap (updatePrior alpha) dms

-- | Estimate the prior alpha from a set of Dirichlet/multinomials
estimatePrior' :: (Real w, Enum a) => [Multinom w a] -> Alpha a -> Alpha a
estimatePrior' dms alpha =
  let domain = toList $ dmDomain $ head dms
      f k = let num = sum $ map (\i->digamma (realToFrac (dmGetCounts i k) + alphaOf alpha k)
                                      - digamma (alphaOf alpha k)
                                )
                      $ filter (\i->dmGetCounts i k > 0) dms
                total i = realToFrac $ sum $ map (\k->dmGetCounts i k) domain
                sumAlpha = sum $ map (alphaOf alpha) domain
                denom = sum $ map (\i->digamma (total i + sumAlpha) - digamma sumAlpha) dms
            in case () of
                 _ | isNaN num  -> error $ "BayesStack.DirMulti.estimatePrior': num = NaN: "++show (map (\i->(digamma (realToFrac (dmGetCounts i k) + alphaOf alpha k), digamma (alphaOf alpha k))) dms)
                 _ | denom == 0 -> error "BayesStack.DirMulti.estimatePrior': denom=0"
                 _ | isInfinite num -> error "BayesStack.DirMulti.estimatePrior': num is infinity "
                 _ | isNaN (alphaOf alpha k * num / denom) -> error $ "NaN"++show (num, denom)
                 otherwise  -> alphaOf alpha k * num / denom
  in asymAlpha $ foldMap (\k->EM.singleton k (f k)) domain

estimatePrior :: (Real w, Enum a) => Double -> [Multinom w a] -> Alpha a
estimatePrior tol dms = iter $ dmAlpha $ head dms
  where iter alpha = let alpha' = estimatePrior' dms alpha
                         (_, prec)  = alphaToMeanPrecision alpha
                         (_, prec') = alphaToMeanPrecision alpha'
                     in if abs ((prec' - prec) / prec) > tol
                           then iter alpha'
                           else alpha'
