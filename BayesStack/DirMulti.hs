{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             Multinom, dirMulti, symDirMulti, multinom
                             -- | Do not do record updates with these
                           , dmTotal, dmAlpha
                           , decMultinom, incMultinom
                           , prettyMultinom
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

import Data.Number.LogFloat hiding (realToFrac, isNaN, isInfinite)
import Numeric.Digamma
import Math.Gamma

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
                      Alpha {} -> product $ map (\a->logToLogFloat $ checkNaN ("alphaNorm.normNum(asym) alpha="++show a) $ lnGamma a)
                                  $ EM.elems $ aAlphas alpha
                      SymAlpha {} -> logToLogFloat $ checkNaN "alphaNorm.normNum(sym)" $ dim * lnGamma (aAlpha alpha)
        normDenom = logToLogFloat $ checkNaN "alphaNorm.normDenom" $ lnGamma $ sumAlpha alpha

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

-- | 'symMultinomFromPrecision d p' is a symmetric Dirichlet/multinomial over a
-- domain 'd' with precision 'p'
symDirMultiFromPrecision :: Enum a => [a] -> Precision -> Multinom a
symDirMultiFromPrecision domain prec = symDirMulti (0.5*prec) domain

-- | 'dirMultiFromMeanPrecision m p' is an asymmetric Dirichlet/multinomial
-- over a domain 'd' with mean 'm' and precision 'p'
dirMultiFromPrecision :: Enum a => Mean a -> Precision -> Multinom a
dirMultiFromPrecision m p = dirMultiFromAlpha $ meanPrecisionToAlpha m p

-- | Create a symmetric Dirichlet/multinomial
symDirMulti :: Enum a => Double -> [a] -> Multinom a
symDirMulti alpha domain = dirMultiFromAlpha a
  where a = SymAlpha { aDomain = SQ.fromList domain
                     , aAlpha = alpha
                     , aNorm = alphaNorm a
                     }

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
        in 1 / aNorm alpha
           * product (map f $ toList $ dmDomain dm)
           / logToLogFloat (checkNaN "likelihood" $ lnGamma $ realToFrac (dmTotal dm) + sumAlpha alpha) 
  {-# INLINEABLE likelihood #-}

instance FullConditionable Multinom where
  type FCContext Multinom a = (Ord a, Enum a)
  sampleProb dm@(Multinom {dmProbs=prob}) k = prob EM.! k
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
prettyMultinom n showA dm@(Multinom {}) = error "TODO: prettyMultinom"
prettyMultinom n showA dm@(DirMulti {}) =
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
updatePrior :: (Alpha a -> Alpha a) -> Multinom a -> Multinom a
updatePrior f dm@(Multinom {}) = error "TODO: updatePrior"
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

