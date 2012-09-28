{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             Multinom, dirMulti, symDirMulti, multinom
                             -- | Do not do record updates with these
                           , dmTotal, dmAlpha
                           , setMultinom, SetUnset (..)
                           , decMultinom, incMultinom
                           , prettyMultinom
                           , updatePrior
                             -- * Parameter estimation
                           , estimatePrior, reestimatePriors, reestimateSymPriors
                           ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import qualified Data.Foldable 
import Data.Foldable (toList, Foldable, foldMap)
import Data.Function (on)

import Text.PrettyPrint
import Text.Printf

import GHC.Generics (Generic)
import Data.Serialize
import Data.Serialize.EnumMap ()
import Data.Serialize.LogFloat ()

import BayesStack.Core
import BayesStack.Dirichlet

import Data.Number.LogFloat hiding (realToFrac, isNaN, isInfinite)
import Numeric.Digamma
import Math.Gamma hiding (p)

-- | Make error handling a bit easier
checkNaN :: RealFloat a => String -> a -> a
checkNaN loc x | isNaN x = error $ "BayesStack.DirMulti."++loc++": Not a number"
checkNaN loc x | isInfinite x = error $ "BayesStack.DirMulti."++loc++": Infinity"
checkNaN _ x = x

maybeInc, maybeDec :: Maybe Int -> Maybe Int
maybeInc Nothing = Just 1
maybeInc (Just n) = Just (n+1)
maybeDec Nothing = error "Can't decrement zero count"
maybeDec (Just 1) = Nothing
maybeDec (Just n) = Just (n-1)
                  
{-# INLINEABLE decMultinom #-}
{-# INLINEABLE incMultinom #-}
decMultinom, incMultinom :: (Ord a, Enum a) => a -> Multinom a -> Multinom a
decMultinom k dm = dm { dmCounts = EM.alter maybeDec k $ dmCounts dm
                      , dmTotal = dmTotal dm - 1 }
incMultinom k dm = dm { dmCounts = EM.alter maybeInc k $ dmCounts dm
                      , dmTotal = dmTotal dm + 1 }

data SetUnset = Set | Unset
         
setMultinom :: (Enum a, Ord a) => SetUnset -> a -> Multinom a -> Multinom a         
setMultinom Set   s = incMultinom s
setMultinom Unset s = decMultinom s

-- | 'Multinom a' represents multinomial distribution over domain 'a'.
-- Optionally, this can include a collapsed Dirichlet prior.
-- 'Multinom alpha count total' is a multinomial with Dirichlet prior
-- with symmetric parameter 'alpha', ...
data Multinom a = DirMulti { dmAlpha :: Alpha a
                           , dmCounts :: EnumMap a Int
                           , dmTotal :: !Int
                           , dmDomain :: Seq a
                           }
                | Multinom { dmProbs :: !(EnumMap a Double)
                           , dmCounts :: !(EnumMap a Int)
                           , dmTotal :: !Int
                           , dmDomain :: !(Seq a)
                           }
                deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (Multinom a)

-- | 'symMultinomFromPrecision d p' is a symmetric Dirichlet/multinomial over a
-- domain 'd' with precision 'p'
symDirMultiFromPrecision :: Enum a => [a] -> DirPrecision -> Multinom a
symDirMultiFromPrecision domain prec = symDirMulti (0.5*prec) domain

-- | 'dirMultiFromMeanPrecision m p' is an asymmetric Dirichlet/multinomial
-- over a domain 'd' with mean 'm' and precision 'p'
dirMultiFromPrecision :: Enum a => DirMean a -> DirPrecision -> Multinom a
dirMultiFromPrecision m p = dirMultiFromAlpha $ meanPrecisionToAlpha m p

-- | Create a symmetric Dirichlet/multinomial
symDirMulti :: Enum a => Double -> [a] -> Multinom a
symDirMulti alpha domain = dirMultiFromAlpha $ symAlpha domain alpha

-- | A multinomial without a prior
multinom :: Enum a => [(a,Double)] -> Multinom a
multinom probs = Multinom { dmProbs = EM.fromList probs
                          , dmCounts = EM.empty
                          , dmTotal = 0
                          , dmDomain = SQ.fromList $ map fst probs
                          }

-- | Create an asymmetric Dirichlet/multinomial from items and alphas
dirMulti :: Enum a => [(a,Double)] -> Multinom a
dirMulti domain = dirMultiFromAlpha $ asymAlpha $ EM.fromList domain

-- | Create a Dirichlet/multinomial with a given prior
dirMultiFromAlpha :: Enum a => Alpha a -> Multinom a
dirMultiFromAlpha alpha = DirMulti { dmAlpha = alpha
                                   , dmCounts = EM.empty
                                   , dmTotal = 0
                                   , dmDomain = alphaDomain alpha
                                   }

dmGetCounts :: Enum a => Multinom a -> a -> Int
dmGetCounts dm k =
  EM.findWithDefault 0 k (dmCounts dm)

instance HasLikelihood Multinom where
  type LContext Multinom a = (Ord a, Enum a)
  likelihood dm@(Multinom {}) =
    product $ map (\(k,n)->(realToFrac $ dmProbs dm EM.! k)^n) $ EM.assocs $ dmCounts dm
  likelihood dm =
        let alpha = dmAlpha dm
            f k = logToLogFloat $ checkNaN "likelihood(factor)"
                  $ lnGamma (realToFrac (dmGetCounts dm k) + alpha `alphaOf` k)
        in 1 / alphaNormalizer alpha
           * product (map f $ toList $ dmDomain dm)
           / logToLogFloat (checkNaN "likelihood" $ lnGamma $ realToFrac (dmTotal dm) + sumAlpha alpha) 
  {-# INLINEABLE likelihood #-}

instance FullConditionable Multinom where
  type FCContext Multinom a = (Ord a, Enum a)
  sampleProb (Multinom {dmProbs=prob}) k = prob EM.! k
  sampleProb dm@(DirMulti {dmAlpha=a}) k =
  	let alpha = a `alphaOf` k
            n = realToFrac $ dmGetCounts dm k
            total = realToFrac $ dmTotal dm
        in (n + alpha) / (total + sumAlpha a)
  {-# INLINEABLE sampleProb #-}

{-# INLINEABLE probabilities #-}
probabilities :: (Ord a, Enum a) => Multinom a -> Seq (Double, a)
probabilities dm = fmap (\a->(sampleProb dm a, a)) $ dmDomain dm -- FIXME

prettyMultinom :: (Ord a, Enum a) => Int -> (a -> String) -> Multinom a -> Doc
prettyMultinom _ _ (Multinom {})         = error "TODO: prettyMultinom"
prettyMultinom n showA dm@(DirMulti {})  =
  text "DirMulti" <+> parens (text "alpha=" <> prettyAlpha showA (dmAlpha dm))
  $$ nest 5 (fsep $ punctuate comma
            $ map (\(p,a)->text (showA a) <> parens (text $ printf "%1.2e" p))
            $ take n $ Data.Foldable.toList
            $ SQ.sortBy (flip (compare `on` fst)) $ probabilities dm)

-- | Update the prior of a Dirichlet/multinomial
updatePrior :: (Alpha a -> Alpha a) -> Multinom a -> Multinom a
updatePrior _ (Multinom {}) = error "TODO: updatePrior"
updatePrior f dm = dm {dmAlpha=f $ dmAlpha dm}

-- | Relative tolerance in precision for prior estimation
estimationTol = 1e-8

reestimatePriors :: (Foldable f, Functor f, Enum a) => f (Multinom a) -> f (Multinom a)
reestimatePriors dms =
  let usableDms = filter (\dm->dmTotal dm > 5) $ toList dms
      alpha = case () of
                _ | length usableDms <= 3 -> id
                otherwise -> const $ estimatePrior estimationTol usableDms
  in fmap (updatePrior alpha) dms

reestimateSymPriors :: (Foldable f, Functor f, Enum a) => f (Multinom a) -> f (Multinom a)
reestimateSymPriors dms =
  let usableDms = filter (\dm->dmTotal dm > 5) $ toList dms
      alpha = case () of
                _ | length usableDms <= 3 -> id
                otherwise -> const $ symmetrizeAlpha $ estimatePrior estimationTol usableDms
  in fmap (updatePrior alpha) dms

-- | Estimate the prior alpha from a set of Dirichlet/multinomials
estimatePrior' :: (Enum a) => [Multinom a] -> Alpha a -> Alpha a
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

estimatePrior :: (Enum a) => Double -> [Multinom a] -> Alpha a
estimatePrior tol dms = iter $ dmAlpha $ head dms
  where iter alpha = let alpha' = estimatePrior' dms alpha
                         (_, prec)  = alphaToMeanPrecision alpha
                         (_, prec') = alphaToMeanPrecision alpha'
                     in if abs ((prec' - prec) / prec) > tol
                           then iter alpha'
                           else alpha'

