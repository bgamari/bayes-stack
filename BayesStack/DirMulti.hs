{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             DirMulti(..), dirMulti, symDirMulti
                           , decDirMulti, incDirMulti
                           , prettyDirMulti
                           ) where

import qualified Data.Foldable 

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
  
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

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
data DirMulti a = SymDirMulti { dmAlpha :: Alpha
                              , dmCounts :: EnumMap a Int
                              , dmTotal :: Int
                              , dmDomain :: Seq a
                              }
                | DirMulti { dmAlphas :: EnumMap a Alpha
                           , dmCounts :: EnumMap a Int
                           , dmTotal :: Int
                           , dmDomain :: Seq a
                           }
                  deriving (Show, Eq, Generic)

type Alpha = Double

instance (Enum a, Serialize a) => Serialize (DirMulti a)

-- | Create a symmetric Dirichlet/multinomial pair
symDirMulti :: Alpha -> [a] -> DirMulti a
symDirMulti alpha range = SymDirMulti { dmAlpha = alpha
                                      , dmCounts = EM.empty
                                      , dmTotal = 0
                                      , dmDomain = SQ.fromList range
                                      }

-- | Create an asymmetric Dirichlet/multinomial pair
dirMulti :: Enum a => [(a,Alpha)] -> [a] -> DirMulti a
dirMulti alpha range 
  | length alpha /= length range = error "Length of dirMulti prior must equal dimensionality of distribution"
  | otherwise = DirMulti { dmAlphas = EM.fromList alpha
                         , dmCounts = EM.empty
                         , dmTotal = 0
                         , dmDomain = SQ.fromList range
                         }


instance ProbDist DirMulti where
  type PdContext DirMulti a = (Ord a, Enum a)

  prob dm@(SymDirMulti {dmAlpha=alpha, dmCounts=counts, dmTotal=total}) k =
  	let c = realToFrac $ EM.findWithDefault 0 k counts
            range = realToFrac $ SQ.length $ dmDomain dm
        in (c + alpha) / (realToFrac total + range * alpha)

  prob dm@(DirMulti {dmCounts=counts, dmTotal=total}) k =
  	let alpha = dmAlphas dm EM.! k
            c = realToFrac $ EM.findWithDefault 0 k counts
            range = realToFrac $ SQ.length $ dmDomain dm
        in (c + alpha) / (realToFrac total + range * alpha)
  {-# INLINEABLE prob #-}

instance PretendableProbDist DirMulti where
  type PpdContext DirMulti a = (Ord a, Enum a)

  probPretend dm@(SymDirMulti {dmAlpha=alpha, dmCounts=counts, dmTotal=total}) k =
  	let c = realToFrac $ EM.findWithDefault 0 k counts
            range = realToFrac $ SQ.length $ dmDomain dm
        in (c + alpha + 1) / (realToFrac total + range * alpha + 1)

  probPretend dm@(DirMulti {dmCounts=counts, dmTotal=total}) k =
  	let alpha = dmAlphas dm EM.! k
            c = realToFrac $ EM.findWithDefault 0 k counts
            range = realToFrac $ SQ.length $ dmDomain dm
        in (c + alpha + 1) / (realToFrac total + range * alpha + 1)
  {-# INLINEABLE probPretend #-}

{-# INLINEABLE probabilities #-}
probabilities :: (Ord a, Enum a) => DirMulti a -> Seq (Double, a)
probabilities dm = fmap (\a->(prob dm a, a)) $ dmDomain dm

prettyDirMulti :: (Ord a, Enum a, Show a) => Int -> (a -> String) -> DirMulti a -> Doc
prettyDirMulti n showA dm =
  text "DirMulti" <+> parens (text "alpha=" <> text (show $ dmAlpha dm))
  <+> hsep (punctuate comma
            $ map (\(p,a)->text (showA a) <> parens (text $ printf "%1.2e" p))
            $ take n $ Data.Foldable.toList
            $ SQ.sortBy (flip (compare `on` fst)) $ probabilities dm)

