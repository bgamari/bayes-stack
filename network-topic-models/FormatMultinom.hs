{-# LANGUAGE OverloadedStrings #-}

module FormatMultinom ( formatMultinom
                      , formatMultinoms
                      ) where

import           Data.Foldable
import           Data.Monoid

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder.RealFloat

import qualified Data.Map as M

import           BayesStack.Multinomial

formatMultinom :: (Real w, Ord a, Enum a)
               => (a -> TB.Builder) -> Maybe Int -> Multinom w a -> TB.Builder
formatMultinom show n = foldMap formatElem . takeTop . toList . decProbabilities
    where formatElem (p,x) =
               "\t" <> show x <> "\t" <> formatRealFloat Exponent (Just 3) p <> "\n"
          takeTop = maybe id take n

formatMultinoms :: (Real w, Ord k, Ord a, Enum a)
                => (k -> TB.Builder) -> (a -> TB.Builder) -> Maybe Int
                -> M.Map k (Multinom w a) -> TB.Builder
formatMultinoms showKey showElem n = foldMap go . M.assocs
    where go (k,v) = showKey k <> "\n"
                   <> formatMultinom showElem n v <> "\n"
