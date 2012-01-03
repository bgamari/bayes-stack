{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds #-}

module BayesStack.DirMulti ( -- * Dirichlet/multinomial pair
                             DirMulti, symDirMulti
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
data DirMulti a = SymDirMulti { dmAlpha :: Double 
                              , dmCounts :: EnumMap a Int
                              , dmTotal :: Int
                              , dmRange :: Seq a
                              }
                  deriving (Show, Eq)                           

type Alpha = Double

symDirMulti :: Alpha -> [a] -> DirMulti a
symDirMulti alpha range = SymDirMulti { dmAlpha = alpha
                                      , dmCounts = EM.empty
                                      , dmTotal = 0
                                      , dmRange = SQ.fromList range
                                      }


instance ProbDist DirMulti where
  type PdContext DirMulti a = (Ord a, Enum a)
  prob dm@(SymDirMulti {dmAlpha=alpha, dmCounts=counts, dmTotal=total}) k =
  	let c = realToFrac $ EM.findWithDefault 0 k counts
            range = realToFrac $ SQ.length $ dmRange dm
        in (c + alpha + 1) / (realToFrac total + range * alpha + 1)
  {-# INLINEABLE prob #-}

{-# INLINEABLE probabilities #-}
probabilities :: (Ord a, Enum a) => DirMulti a -> Seq (Double, a)
probabilities dm = fmap (\a->(prob dm a, a)) $ dmRange dm

prettyDirMulti :: (Ord a, Enum a, Show a) => Int -> DirMulti a -> Doc
prettyDirMulti n dm =
  text "DirMulti" <+> parens (text "alpha=" <> text (show $ dmAlpha dm))
  <+> hsep (punctuate comma
            $ map (\(p,a)->text (show a) <> parens (text $ printf "%1.2f" p))
            $ take n $ Data.Foldable.toList $ probabilities dm)

