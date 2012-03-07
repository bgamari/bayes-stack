{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             DirMulti, dirMulti, symDirMulti, fixedDirMulti
                             -- | Do not do record updates with these
                           , dmTotal, dmAlpha
                           , decDirMulti, incDirMulti
                           , prettyDirMulti
                           , updatePrior
                             -- * Prior parameter
                           , Alpha(SymAlpha, aAlpha)
                           , alphaOf, setAlphaOf, setSymAlpha
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

import Data.Number.LogFloat hiding (realToFrac)
import Numeric.Digamma
import Math.Gamma

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
                        , dmProbs :: !(EnumMap a Double)
                        , dmCounts :: !(EnumMap a Int)
                        , dmTotal :: !Int
                        , dmDomain :: !(Seq a)
                        }
                  deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (DirMulti a)

instance Serialize LogFloat where
  put = put . (logFromLogFloat :: LogFloat -> Double)
  get = (logToLogFloat :: Double -> LogFloat) `fmap` get

-- | A Dirichlet prior
data Alpha a = SymAlpha { aDomain :: Seq a
                        , aAlpha :: !Double
                        , aNorm :: LogFloat
                        }
             | Alpha { aAlphas :: EnumMap a Double
                     , aSumAlphas :: !Double
                     , aNorm :: LogFloat
                     }
             deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (Alpha a)

type Mean a = EnumMap a Double
type Precision = Double

-- | Construct an asymmetric Alpha
asymAlpha :: Enum a => EnumMap a Double -> Alpha a
asymAlpha alphas = Alpha { aAlphas = alphas
                         , aSumAlphas = sum $ EM.elems alphas
                         , aNorm = alphaNorm $ asymAlpha alphas
                         }

setSymAlpha :: Enum a => Double -> Alpha a -> Alpha a
setSymAlpha alpha a = let b = (symmetrizeAlpha a) { aAlpha = alpha
                                                  , aNorm = alphaNorm b
                                                  }
                      in b

-- | Compute the normalizer of the likelihood involving alphas,
-- (product_k gamma(alpha_k)) / gamma(sum_k alpha_k)
alphaNorm :: Enum a => Alpha a -> LogFloat
alphaNorm alpha = normNum / normDenom
  where dim = realToFrac $ SQ.length $ aDomain alpha
        normNum = case alpha of
                      Alpha {} -> product $ map (logToLogFloat . lnGamma) $ EM.elems $ aAlphas alpha
                      SymAlpha {} -> logToLogFloat $ dim * lnGamma (aAlpha alpha)
        normDenom = logToLogFloat $ lnGamma $ sumAlpha alpha

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
symmetrizeAlpha alpha@(Alpha {}) =
  SymAlpha { aDomain = alphaDomain alpha
           , aAlpha = sumAlpha alpha / realToFrac (EM.size $ aAlphas alpha)
           , aNorm = alphaNorm $ symmetrizeAlpha alpha
           }

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
symDirMulti alpha domain = dirMultiFromAlpha a
  where a = SymAlpha { aDomain = SQ.fromList domain
                     , aAlpha = alpha
                     , aNorm = alphaNorm a
                     }

fixedDirMulti :: Enum a => [(a,Double)] -> DirMulti a
fixedDirMulti probs = Fixed { dmAlpha = a
                            , dmProbs = EM.fromList probs
                            , dmCounts = EM.empty
                            , dmTotal = 0
                            , dmDomain = SQ.fromList $ map fst probs
                            }
  where a = SymAlpha { aDomain = SQ.fromList $ map fst probs
                     , aAlpha = 0
                     , aNorm = alphaNorm a
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

instance HasLikelihood DirMulti where
  type LContext DirMulti a = (Ord a, Enum a)
  likelihood dm@(Fixed {}) =
    product $ map (\(k,n)->(realToFrac $ dmProbs dm EM.! k)^n) $ EM.assocs $ dmCounts dm
  likelihood dm =
        let alpha = dmAlpha dm
            f (k,n) = logToLogFloat $ lnGamma (realToFrac n + alpha `alphaOf` k)
        in 1 / aNorm alpha
           * product (map f $ EM.assocs $ dmCounts dm)
           / logToLogFloat (lnGamma $ realToFrac (dmTotal dm) + sumAlpha alpha) 
  {-# INLINEABLE likelihood #-}

instance FullConditionable DirMulti where
  type FCContext DirMulti a = (Ord a, Enum a)
  sampleProb dm@(Fixed {dmProbs=prob}) k = prob EM.! k
  sampleProb dm@(DirMulti {dmAlpha=a}) k =
  	let alpha = a `alphaOf` k
            n = realToFrac $ dmGetCounts dm k
            total = realToFrac $ dmTotal dm
        in (n + alpha) / (total + sumAlpha a)
  {-# INLINEABLE sampleProb #-}

{-# INLINEABLE probabilities #-}
probabilities :: (Ord a, Enum a) => DirMulti a -> Seq (Double, a)
probabilities dm = fmap (\a->(sampleProb dm a, a)) $ dmDomain dm -- FIXME

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
estimationTol = 1e-8

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
                total i = realToFrac $ sum $ map (\k->dmGetCounts i k) domain
                sumAlpha = sum $ map (alphaOf alpha) domain
                denom = sum $ map (\i->digamma (total i + sumAlpha) - digamma sumAlpha) dms
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

