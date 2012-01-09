{-# LANGUAGE TypeFamilies #-}

module BayesStack.Core.GibbsUpdate ( GibbsUpdateUnit(..)
                                   , gibbsUpdate, gibbsUpdateOne
                                   , concurrentGibbsUpdate
                                   ) where

import Data.Random
import qualified Data.Random.Distribution.Categorical as C
import Data.Random.Sequence

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Foldable

import Control.Monad
import Control.Monad.IO.Class

import BayesStack.Core.ModelMonad
import BayesStack.Core.Types
import BayesStack.Core.Concurrent


class GibbsUpdateUnit unit where
  type GUValue unit :: *
  guUnset :: unit -> ModelMonad ()
  guDomain :: unit -> ModelMonad [GUValue unit]
  guProb :: unit -> GUValue unit -> ModelMonad Probability
  guSet :: unit -> GUValue unit -> ModelMonad ()

gibbsUpdate :: GibbsUpdateUnit unit => unit -> ModelMonad ()
gibbsUpdate unit = 
  do guUnset unit
     r <- guDomain unit
     probs <- forM r $ guProb unit
     let dist = C.fromWeightedList $ zip probs r
     new <- sample dist
     guSet unit new
    
-- | Randomly sample one of a group of units
gibbsUpdateOne :: GibbsUpdateUnit unit => Seq unit -> ModelMonad ()
gibbsUpdateOne units = liftRVar (randomElementT units) >>= gibbsUpdate

concurrentGibbsUpdate :: GibbsUpdateUnit unit => Int -> Seq unit -> ModelMonad ()
concurrentGibbsUpdate nSweeps units =
   do let chunks = chunk numCapabilities units
      concurrentRunModels $ map (\c->do replicateM (nSweeps*SQ.length c) $ gibbsUpdateOne c
                                        return ()
                                ) $ toList chunks

