{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             DirMulti(..), dirMulti, symDirMulti
                           , decDirMulti, incDirMulti
                           , prettyDirMulti
                             -- * Prior parameter
                           , Alpha(..)
                           , alphaToMeanPrecision, meanPrecisionToAlpha, symmetrizeAlpha
                             -- * Parameter estimation
                           , estimatePrior, reestimatePriors, reestimateSymPriors
                           ) where

import qualified Data.Foldable 

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.Foldable (toList, Foldable)
import Data.List (sortBy)
import Data.Function (on)

import Text.PrettyPrint
import Text.Printf

import GHC.Generics
import Data.Serialize
import Data.Serialize.EnumMap
 
import BayesStack.Core

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

data Alpha a = SymAlpha (Seq a) Double
             | Alpha (EnumMap a Double)
             deriving (Show, Eq, Generic)
instance (Enum a, Serialize a) => Serialize (Alpha a)

type Mean a = EnumMap a Double
type Precision = Double

alphaOf :: Enum a => Alpha a -> a -> Double
alphaOf (SymAlpha _ alpha) = const $ alpha
alphaOf (Alpha alpha) = (alpha EM.!)

alphaToMeanPrecision :: Enum a => Alpha a -> (Mean a, Precision)
alphaToMeanPrecision (SymAlpha domain alpha) =
  let prec = realToFrac (SQ.length domain) * alpha
  in (EM.fromList $ map (\a->(a, alpha/prec)) $ toList domain, prec)
alphaToMeanPrecision (Alpha alpha) = let prec = sum $ EM.elems alpha
                                     in (fmap (/prec) alpha, prec)

meanPrecisionToAlpha :: (Mean a, Precision) -> Alpha a
meanPrecisionToAlpha (mean,prec) = Alpha $ fmap (*prec) mean

symmetrizeAlpha :: Enum a => Alpha a -> Alpha a
symmetrizeAlpha alpha@(SymAlpha {}) = alpha
symmetrizeAlpha (Alpha a) = SymAlpha domain alpha
  where domain = SQ.fromList $ EM.keys a
        alpha = sum (EM.elems a) / realToFrac (EM.size a)

symDirMultiFromPrecision :: [a] -> Precision -> DirMulti a
symDirMultiFromPrecision domain prec = symDirMulti (0.5*prec) domain

-- | Create a symmetric Dirichlet/multinomial pair
symDirMulti :: Double -> [a] -> DirMulti a
symDirMulti alpha domain = let domain' = SQ.fromList domain
                           in DirMulti { dmAlpha = SymAlpha domain' alpha
                                       , dmCounts = EM.empty
                                       , dmTotal = 0
                                       , dmDomain = domain'
                                       }


-- | Create an asymmetric Dirichlet/multinomial pair
dirMulti :: Enum a => [(a,Double)] -> [a] -> DirMulti a
dirMulti alpha domain
  | length alpha /= length domain = error "Length of dirMulti prior must equal dimensionality of distribution"
  | otherwise = DirMulti { dmAlpha = Alpha $ EM.fromList alpha
                         , dmCounts = EM.empty
                         , dmTotal = 0
                         , dmDomain = SQ.fromList domain
                         }


instance ProbDist DirMulti where
  type PdContext DirMulti a = (Ord a, Enum a)

  prob dm@(DirMulti {dmCounts=counts, dmTotal=total}) k =
  	let alpha = (dmAlpha dm) `alphaOf` k
            c = realToFrac $ EM.findWithDefault 0 k counts
            range = realToFrac $ SQ.length $ dmDomain dm
        in (c + alpha) / (realToFrac total + range * alpha)
  {-# INLINEABLE prob #-}

instance PretendableProbDist DirMulti where
  type PpdContext DirMulti a = (Ord a, Enum a)

  probPretend dm@(DirMulti {dmCounts=counts, dmTotal=total}) k =
  	let alpha = (dmAlpha dm) `alphaOf` k
            c = realToFrac $ EM.findWithDefault 0 k counts
            range = realToFrac $ SQ.length $ dmDomain dm
        in (c + alpha + 1) / (realToFrac total + range * alpha + 1)
  {-# INLINEABLE probPretend #-}

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

prettyAlpha :: (a -> String) -> Alpha a -> Doc
prettyAlpha showA (SymAlpha _ alpha) = text "Symmetric" <+> double alpha
prettyAlpha showA (Alpha alpha) = text "Assymmetric"

reestimatePriors :: (Foldable f, Functor f, Enum a) => f (DirMulti a) -> f (DirMulti a)
reestimatePriors dms =
  let alpha = estimatePrior $ toList dms
  in fmap (\dm->dm {dmAlpha=alpha}) dms

reestimateSymPriors :: (Foldable f, Functor f, Enum a) => f (DirMulti a) -> f (DirMulti a)
reestimateSymPriors dms =
  let alpha = symmetrizeAlpha $ estimatePrior $ toList dms
  in fmap (\dm->dm {dmAlpha=alpha}) dms

-- | Number of iterations to run in prior estimation
nEstimationIters = 1000

-- | Estimate the prior alpha from a Dirichlet/multinomial
-- Based on Andrew Mccallum's interpretation of Tom Minka's implementation
estimatePrior :: (Enum a) => [DirMulti a] -> Alpha a
estimatePrior dms =
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
            newAlpha hist x =
              let (n,_) = EM.findMax hist
                  digammas n = tail $ scanl (\digamma i -> digamma + 1/(x + realToFrac i - 1)) 0 [1..n]
              in realToFrac $ sum
                 $ zipWith (\digamma i->realToFrac (EM.findWithDefault 0 i hist)
                                        * realToFrac digamma)
                 (digammas n) [1..n]
            --alpha' :: Enum a => a -> Alpha -> Alpha
            alpha' k alpha = let num = newAlpha (binHist EM.! k) (alphas EM.! k)
                                 denom = newAlpha lengthHist (sum $ EM.elems alphas)
                             in alpha * num /  denom
        in EM.mapWithKey alpha' alphas
      alphas0 = EM.fromList $ zip domain $ map (alphaOf (dmAlpha $ head dms)) domain
  in Alpha $ head $ drop nEstimationIters $ iterate f alphas0
{-# INLINEABLE estimatePrior #-}

