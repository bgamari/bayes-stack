{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module BayesStack.Core.GibbsUpdate ( GibbsUpdateUnit(..)
                                   , fullGibbsUpdate, gibbsUpdate, gibbsUpdateOne
                                   , concurrentGibbsUpdate, concurrentFullGibbsUpdate
                                   , GibbsUpdateState, newGibbsUpdateState
                                   ) where

import Data.Random
import qualified Data.Random.Distribution.Categorical as C
import Data.Random.Sequence

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Foldable hiding (sum)

import Control.Monad (forM, replicateM_)

import Data.Number.LogFloat hiding (realToFrac)
import BayesStack.Core.ModelMonad
import BayesStack.Core.Types
import BayesStack.Core.Concurrent
import BayesStack.Core.Shared

type Weight = Double

categoricalDraw :: (Distribution Uniform w, Num w, Ord w) => w -> [a] -> (a -> ModelMonad w) -> ModelMonad (Maybe a)
categoricalDraw norm as weightOf  =
  let f b (a:rest) =
        do w <- weightOf a
           case () of
              _ | b <= w    -> return $ Just a
              _ | otherwise -> f (b-w) rest
      f _ [] = return Nothing
  in do b <- liftRVar $ uniform 0 norm
        f b as

data GibbsUpdateState = GibbsUpdateState { gusNorm :: Maybe Weight
                                         }
                      deriving (Show)

newGibbsUpdateState :: ModelMonad (Shared GibbsUpdateState)
newGibbsUpdateState = newShared $ GibbsUpdateState { gusNorm=Nothing }

class GibbsUpdateUnit unit where
  type GUValue unit :: *
  guUnset :: unit -> ModelMonad (GUValue unit)
  guDomain :: unit -> ModelMonad [GUValue unit]
  guProb :: unit -> GUValue unit -> ModelMonad Double
  guSet :: unit -> GUValue unit -> ModelMonad ()
  guState :: unit -> Shared GibbsUpdateState

fullGibbsUpdate :: GibbsUpdateUnit unit => unit -> ModelMonad ()
fullGibbsUpdate unit =
  do old <- guUnset unit
     domain <- guDomain unit
     probs <- forM domain $ guProb unit
     let norm = sum probs
     updateShared (guState unit) $ \s->s {gusNorm=Just norm}
     new <- liftRVar $ sample $ C.fromList $ zip (map (\p->p/norm) probs) domain
     guSet unit new

gibbsUpdate :: GibbsUpdateUnit unit => unit -> ModelMonad ()
gibbsUpdate unit =
  do state <- getShared $ guState unit
     case gusNorm state of
          Just norm -> do
            old <- guUnset unit
            domain <- guDomain unit
            new <- categoricalDraw norm domain (guProb unit)
            case new of
              Just new' -> guSet unit new'
              Nothing   -> guSet unit old >> fullGibbsUpdate unit
          Nothing -> fullGibbsUpdate unit
    
-- | Randomly sample one of a group of units
gibbsUpdateOne :: GibbsUpdateUnit unit => Seq unit -> ModelMonad ()
gibbsUpdateOne units = liftRVar (randomElementT units) >>= gibbsUpdate

concurrentGibbsUpdate :: GibbsUpdateUnit unit => Int -> Seq unit -> ModelMonad ()
concurrentGibbsUpdate nSweeps units =
   do let chunks = chunk numCapabilities units
      concurrentRunModels $ map (\c->do forM_ c fullGibbsUpdate 
                                        replicateM_ (nSweeps*SQ.length c) $ gibbsUpdateOne c
                                        return ()
                                ) $ toList chunks

fullGibbsUpdateOne :: GibbsUpdateUnit unit => Seq unit -> ModelMonad ()
fullGibbsUpdateOne units = liftRVar (randomElementT units) >>= fullGibbsUpdate

concurrentFullGibbsUpdate :: GibbsUpdateUnit unit => Int -> Seq unit -> ModelMonad ()
concurrentFullGibbsUpdate nSweeps units =
   do let chunks = chunk numCapabilities units
      concurrentRunModels $ map (\c->do replicateM_ (nSweeps*SQ.length c) $ fullGibbsUpdateOne c
                                        return ()
                                ) $ toList chunks
