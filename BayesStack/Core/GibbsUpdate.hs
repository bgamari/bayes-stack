{-# LANGUAGE TypeFamilies #-}

module BayesStack.Core.GibbsUpdate ( GibbsUpdateUnit(..)
                                   , gibbsUpdate, gibbsUpdateOne
                                   , concurrentGibbsUpdate
                                   ) where

import Data.Random
import qualified Data.Random.Distribution.Categorical as C

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
gibbsUpdateOne :: GibbsUpdateUnit unit => [unit] -> ModelMonad ()
gibbsUpdateOne units =
  do a <- sample $ randomElement units
     gibbsUpdate a

concurrentGibbsUpdate :: GibbsUpdateUnit unit => Int -> [unit] -> ModelMonad ()
concurrentGibbsUpdate nIter units =
   do let chunks = chunk numCapabilities units
      concurrentRunModel $ map (\c->do replicateM nIter $ gibbsUpdateOne c
                                       return ()
                               ) chunks
