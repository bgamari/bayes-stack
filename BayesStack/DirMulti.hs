{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             DirMulti, dirMulti, symDirMulti, fixedDirMulti
                             -- | Do not do record updates with these
                           , dmTotal, dmAlpha
                           , decDirMulti, incDirMulti
                           , prettyDirMulti
                           , updatePrior
                             -- * Prior parameter
                           , Alpha
                           , alphaOf, setAlphaOf
                           , alphaToMeanPrecision, meanPrecisionToAlpha
                           , symmetrizeAlpha
                           , prettyAlpha
                             -- * Parameter estimation
                           , estimatePrior, reestimatePriors, reestimateSymPriors
                           ) where

import qualified Data.Foldable 

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.Foldable (toList, Foldable, fold, foldMap)
import Data.Function (on)

import Text.PrettyPrint
import Text.Printf

import GHC.Generics (Generic)
import Data.Serialize
import Data.Serialize.EnumMap
 
import BayesStack.Core

import Numeric.Digamma
import Debug.Trace

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
data DirMulti a = DirMulti { dmAlpha :: Alpha a
                           , dmCounts :: EnumMap a Int
                           , dmTotal :: !Int
                           , dmDomain :: Seq a
                           }
                | Fixed { dmAlpha :: !(Alpha a)
                        , dmProbs :: !(EnumMap a Probability)
                        , dmCounts :: !(EnumMap a Int)
                        , dmTotal :: !Int
                        , dmDomain :: !(Seq a)
                        }
                  deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (DirMulti a)

-- | A Dirichlet prior
data Alpha a = SymAlpha { aDomain :: Seq a
                        , aAlpha :: !Double
                        , aNorm :: !LogFloat
                        }
             | Alpha { aAlphas :: EnumMap a Double
                     , aSumAlphas :: !Double
                     , aNorm :: !LogFloat
                     }
             deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (Alpha a)

type Mean a = EnumMap a Double
type Precision = Double

-- | Construct an asymmetric Alpha
asymAlpha :: Enum a => EnumMap a Double -> Alpha a
asymAlpha alpha = Alpha alpha (sum $ EM.elems alpha)

-- | 'alphaDomain a' is the domain of prior 'a'
alphaDomain :: Enum a => Alpha a -> Seq a
alphaDomain (SymAlpha {aDomain=d}) = d
alphaDomain (Alpha {aAlphas=a}) = SQ.fromList $ EM.keys a

-- | 'alphaOf alpha k' is the value of element 'k' in prior 'alpha'
alphaOf :: Enum a => Alpha a -> a -> Double
alphaOf (SymAlpha {aAlpha=alpha}) = const alpha
alphaOf (Alpha {aAlphas=alphas}) = (alphas EM.!)

-- | 'sumAlpha alpha' is the sum of all alphas
sumAlpha :: Enum a => Alpha a -> Double
sumAlpha (SymAlpha {aDomain=domain, aAlpha=alpha}) = realToFrac (SQ.length domain) * alpha
sumAlpha (Alpha {aSumAlphas=sum}) = sum

-- | Set a particular alpha element
setAlphaOf :: Enum a => a -> Double -> Alpha a -> Alpha a
setAlphaOf k a alpha@(SymAlpha {}) = setAlphaOf k a $ asymmetrizeAlpha alpha
setAlphaOf k a (Alpha {aAlphas=alphas}) = asymAlpha $ EM.insert k a alphas

-- | 'alphaToMeanPrecision a' is the mean/precision representation of the prior 'a'
alphaToMeanPrecision :: Enum a => Alpha a -> (Mean a, Precision)
alphaToMeanPrecision (SymAlpha {aDomain=dom, aAlpha=alpha}) =
  let prec = realToFrac (SQ.length dom) * alpha
  in (EM.fromList $ map (\a->(a, alpha/prec)) $ toList dom, prec)
alphaToMeanPrecision (Alpha {aAlphas=alphas, aSumAlphas=prec}) =
  (fmap (/prec) alphas, prec)

-- | 'meanPrecisionToAlpha m p' is a prior with mean 'm' and precision 'p'
meanPrecisionToAlpha :: Enum a => Mean a -> Precision -> Alpha a
meanPrecisionToAlpha mean prec = asymAlpha $ fmap (*prec) mean

-- | Symmetrize a Dirichlet prior (such that mean=0) 
symmetrizeAlpha :: Enum a => Alpha a -> Alpha a
symmetrizeAlpha alpha@(SymAlpha {}) = alpha
symmetrizeAlpha alpha@(Alpha {}) = SymAlpha (alphaDomain alpha) alpha'
  where alpha' = sumAlphas alpha / realToFrac (EM.size a)

-- | Turn a symmetric alpha into an asymmetric alpha. For internal use.
asymmetrizeAlpha :: Enum a => Alpha a -> Alpha a
asymmetrizeAlpha (SymAlpha {aDomain=domain, aAlpha=alpha}) =
  asymAlpha $ fold $ fmap (\k->EM.singleton k alpha) domain
asymmetrizeAlpha alpha@(Alpha {}) = alpha

-- | 'symDirMultiFromPrecision d p' is a symmetric Dirichlet/multinomial over a
-- domain 'd' with precision 'p'
symDirMultiFromPrecision :: Enum a => [a] -> Precision -> DirMulti a
symDirMultiFromPrecision domain prec = symDirMulti (0.5*prec) domain

-- | 'dirMultiFromMeanPrecision m p' is an asymmetric Dirichlet/multinomial
-- over a domain 'd' with mean 'm' and precision 'p'
dirMultiFromPrecision :: Enum a => Mean a -> Precision -> DirMulti a
dirMultiFromPrecision m p = dirMultiFromAlpha $ meanPrecisionToAlpha m p

-- | Create a symmetric Dirichlet/multinomial
symDirMulti :: Enum a => Double -> [a] -> DirMulti a
symDirMulti alpha domain = dirMultiFromAlpha $ SymAlpha (SQ.fromList domain) alpha

fixedDirMulti :: Enum a => [(a,Probability)] -> DirMulti a
fixedDirMulti probs = Fixed { dmAlpha = SymAlpha (SQ.fromList $ map fst probs) 0
                            , dmProbs = EM.fromList probs
                            , dmCounts = EM.empty
                            , dmTotal = 0
                            , dmDomain = SQ.fromList $ map fst probs
                            }

-- | Create an asymmetric Dirichlet/multinomial from items and alphas
dirMulti :: Enum a => [(a,Double)] -> DirMulti a
dirMulti domain = dirMultiFromAlpha $ asymAlpha $ EM.fromList domain

-- | Create a Dirichlet/multinomial with a given prior
dirMultiFromAlpha :: Enum a => Alpha a -> DirMulti a
dirMultiFromAlpha alpha = DirMulti { dmAlpha = alpha
                                   , dmCounts = EM.empty
                                   , dmTotal = 0
                                   , dmDomain = alphaDomain alpha
                                   }

dmGetCounts :: Enum a => DirMulti a -> a -> Int
dmGetCounts (DirMulti {dmCounts=counts}) k =
  EM.findWithDefault 0 k counts

instance ProbDist DirMulti where
  type PdContext DirMulti a = (Ord a, Enum a)

  prob dm@(Fixed {dmProbs=prob}) k = prob EM.! k
  prob dm@(DirMulti {dmTotal=total}) k =
  	let alpha = (dmAlpha dm) `alphaOf` k
            c = realToFrac $ dmGetCounts dm k
        in (c + alpha) / (realToFrac total + sumAlpha (dmAlpha dm))
  {-# INLINEABLE prob #-}

{-# INLINEABLE probabilities #-}
probabilities :: (Ord a, Enum a) => DirMulti a -> Seq (Double, a)
probabilities dm = fmap (\a->(prob dm a, a)) $ dmDomain dm

prettyDirMulti :: (Ord a, Enum a) => Int -> (a -> String) -> DirMulti a -> Doc
prettyDirMulti n showA dm =
  text "DirMulti" <+> parens (text "alpha=" <> prettyAlpha showA (dmAlpha dm))
  $$ nest 5 (fsep $ punctuate comma
            $ map (\(p,a)->text (showA a) <> parens (text $ printf "%1.2e" p))
            $ take n $ Data.Foldable.toList
            $ SQ.sortBy (flip (compare `on` fst)) $ probabilities dm)

prettyAlpha :: Enum a => (a -> String) -> Alpha a -> Doc
prettyAlpha showA (SymAlpha {aAlpha=alpha}) = text "Symmetric" <+> double alpha
prettyAlpha showA (Alpha {aAlphas=alphas}) =
  text "Assymmetric"
  <+> fsep (punctuate comma
           $ map (\(a,alpha)->text (showA a) <> parens (text $ printf "%1.2e" alpha))
           $ take 100 $ EM.toList $ alphas)

-- | Update the prior of a Dirichlet/multinomial
updatePrior :: (Alpha a -> Alpha a) -> DirMulti a -> DirMulti a
updatePrior f dm = dm {dmAlpha=f $ dmAlpha dm}

-- | Relative tolerance in precision for prior estimation
estimationTol = 1e-3

reestimatePriors :: (Foldable f, Functor f, Enum a) => f (DirMulti a) -> f (DirMulti a)
reestimatePriors dms =
  let alpha = estimatePrior estimationTol $ filter (\dm->dmTotal dm > 5) $ toList dms
  in fmap (updatePrior $ const alpha) dms

reestimateSymPriors :: (Foldable f, Functor f, Enum a) => f (DirMulti a) -> f (DirMulti a)
reestimateSymPriors dms =
  let alpha = symmetrizeAlpha $ estimatePrior estimationTol $ filter (\dm->dmTotal dm > 5) $ toList dms
  in fmap (updatePrior $ const alpha) dms

-- | Estimate the prior alpha from a set of Dirichlet/multinomials
estimatePrior' :: (Enum a) => [DirMulti a] -> Alpha a -> Alpha a
estimatePrior' dms alpha =
  let domain = toList $ dmDomain $ head dms
      f k = let num = sum $ map (\i->digamma (realToFrac (dmGetCounts i k) + alphaOf alpha k)
                                      - digamma (alphaOf alpha k)
                                ) dms
                n i = sum $ map (\k->dmGetCounts i k) domain
                sumAlpha = sum $ map (alphaOf alpha) domain
                denom = sum $ map (\i->digamma (realToFrac (n i) + sumAlpha) - digamma sumAlpha) dms
            in alphaOf alpha k * num / denom
  in asymAlpha $ foldMap (\k->EM.singleton k (f k)) domain

estimatePrior :: (Enum a) => Double -> [DirMulti a] -> Alpha a
estimatePrior tol dms = iter $ dmAlpha $ head dms
  where iter alpha = let alpha' = estimatePrior' dms alpha
                         (_, prec)  = alphaToMeanPrecision alpha
                         (_, prec') = alphaToMeanPrecision alpha'
                     in if abs ((prec' - prec) / prec) > tol
                           then iter alpha'
                           else alpha'

