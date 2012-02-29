{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             DirMulti, dirMulti, symDirMulti
                           , dmTotal
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

import Data.Foldable (toList, Foldable, fold)
import Data.Function (on)

import Text.PrettyPrint
import Text.Printf

import GHC.Generics (Generic)
import Data.Serialize
import Data.Serialize.EnumMap
 
import BayesStack.Core

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
                           , dmTotal :: Int
                           , dmDomain :: Seq a
                           }
                  deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (DirMulti a)

-- | A Dirichlet prior
data Alpha a = SymAlpha (Seq a) Double
             | Alpha (EnumMap a Double) Double
             deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (Alpha a)

type Mean a = EnumMap a Double
type Precision = Double

-- | Construct an asymmetric Alpha
asymAlpha :: Enum a => EnumMap a Double -> Alpha a
asymAlpha alpha = Alpha alpha (sum $ EM.elems alpha)

-- | 'alphaDomain a' is the domain of prior 'a'
alphaDomain :: Enum a => Alpha a -> Seq a
alphaDomain (SymAlpha domain _) = domain
alphaDomain (Alpha alpha _) = SQ.fromList $ EM.keys alpha

-- | 'alphaOf alpha k' is the value of element 'k' in prior 'alpha'
alphaOf :: Enum a => Alpha a -> a -> Double
alphaOf (SymAlpha _ alpha) = const alpha
alphaOf (Alpha alpha _) = (alpha EM.!)

-- | 'sumAlpha alpha' is the sum of all alphas
sumAlpha :: Enum a => Alpha a -> Double
sumAlpha (SymAlpha domain alpha) = realToFrac (SQ.length domain) * alpha
sumAlpha (Alpha alpha sum) = sum

-- | Set a particular alpha element
setAlphaOf :: Enum a => a -> Double -> Alpha a -> Alpha a
setAlphaOf k a alpha@(SymAlpha {}) = setAlphaOf k a $ asymmetrizeAlpha alpha
setAlphaOf k a (Alpha alpha _) = asymAlpha $ EM.insert k a alpha

-- | 'alphaToMeanPrecision a' is the mean/precision representation of the prior 'a'
alphaToMeanPrecision :: Enum a => Alpha a -> (Mean a, Precision)
alphaToMeanPrecision (SymAlpha domain alpha) =
  let prec = realToFrac (SQ.length domain) * alpha
  in (EM.fromList $ map (\a->(a, alpha/prec)) $ toList domain, prec)
alphaToMeanPrecision (Alpha alpha _) = let prec = sum $ EM.elems alpha
                                       in (fmap (/prec) alpha, prec)

-- | 'meanPrecisionToAlpha m p' is a prior with mean 'm' and precision 'p'
meanPrecisionToAlpha :: Enum a => Mean a -> Precision -> Alpha a
meanPrecisionToAlpha mean prec = asymAlpha $ fmap (*prec) mean

-- | Symmetrize a Dirichlet prior (such that mean=0) 
symmetrizeAlpha :: Enum a => Alpha a -> Alpha a
symmetrizeAlpha alpha@(SymAlpha {}) = alpha
symmetrizeAlpha alpha@(Alpha a _) = SymAlpha (alphaDomain alpha) alpha'
  where alpha' = sum (EM.elems a) / realToFrac (EM.size a)

-- | Turn a symmetric alpha into an asymmetric alpha. For internal use.
asymmetrizeAlpha :: Enum a => Alpha a -> Alpha a
asymmetrizeAlpha (SymAlpha domain alpha) = asymAlpha $ fold $ fmap (\k->EM.singleton k alpha) domain
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

instance ProbDist DirMulti where
  type PdContext DirMulti a = (Ord a, Enum a)

  prob dm@(DirMulti {dmCounts=counts, dmTotal=total}) k =
  	let alpha = (dmAlpha dm) `alphaOf` k
            c = realToFrac $ EM.findWithDefault 0 k counts
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
prettyAlpha showA (SymAlpha _ alpha) = text "Symmetric" <+> double alpha
prettyAlpha showA (Alpha alpha _) =
  text "Assymmetric"
  <+> fsep (punctuate comma
           $ map (\(a,alpha)->text (showA a) <> parens (text $ printf "%1.2e" alpha))
           $ take 100 $ EM.toList $ alpha)

-- | Number of iterations to run in prior estimation
nEstimationIters = 1000

-- | Update the prior of a Dirichlet/multinomial
updatePrior :: (Alpha a -> Alpha a) -> DirMulti a -> DirMulti a
updatePrior f dm = dm {dmAlpha=f $ dmAlpha dm}

reestimatePriors :: (Foldable f, Functor f, Enum a) => f (DirMulti a) -> f (DirMulti a)
reestimatePriors dms =
  let alpha = estimatePrior nEstimationIters $ toList dms
  in fmap (\dm->dm {dmAlpha=alpha}) dms

reestimateSymPriors :: (Foldable f, Functor f, Enum a) => f (DirMulti a) -> f (DirMulti a)
reestimateSymPriors dms =
  let alpha = symmetrizeAlpha $ estimatePrior nEstimationIters $ toList dms
  in fmap (\dm->dm {dmAlpha=alpha}) dms

-- | Estimate the prior alpha from a Dirichlet/multinomial
-- Based on Andrew Mccallum's interpretation of Tom Minka's implementation
estimatePrior :: (Enum a) => Int -> [DirMulti a] -> Alpha a
estimatePrior nIter dms =
  let domain = toList $ dmDomain $ head dms
      --binHist :: Enum a => EnumMap a (EnumMap Int Int)
      binHist = EM.map (EM.fromListWith (+))
                $ EM.fromListWith (++) $ do dm <- dms
                                            (a,c) <- EM.toList $ dmCounts dm
                                            return (a, [(c, 1)])
      lengthHist :: EnumMap Int Int
      lengthHist = EM.fromListWith (+) $ do dm <- dms
                                            return (dmTotal dm, 1)
      --f :: Enum a => EnumMap a Alpha -> EnumMap a Alpha
      f alphas =
        let newAlpha :: EnumMap Int Int -> Double -> Double
            newAlpha hist _ | EM.null hist = trace "uh oh" 1e-5 -- Empty histogram
            newAlpha hist x =
              let (n,_) = EM.findMax hist
                  digammas n = tail $ scanl (\digamma i -> digamma + 1/(x + realToFrac i - 1)) 0 [1..n]
              in realToFrac $ sum
                 $ zipWith (\digamma i->realToFrac (EM.findWithDefault 0 i hist)
                                        * realToFrac digamma)
                 (digammas n) [1..n]
            --alpha' :: Enum a => a -> Alpha -> Alpha
            alpha' k alpha = let num = newAlpha (EM.findWithDefault EM.empty k binHist) (alphas EM.! k)
                                 denom = newAlpha lengthHist (sum $ EM.elems alphas)
                             in alpha * num /  denom
        in EM.mapWithKey alpha' alphas
      alphas0 = EM.fromList $ zip domain $ map (alphaOf (dmAlpha $ head dms)) domain
  in asymAlpha $ head $ drop nIter $ iterate f alphas0
{-# INLINEABLE estimatePrior #-}

