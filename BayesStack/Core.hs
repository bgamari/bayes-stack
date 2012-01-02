{-# LANGUAGE TypeFamilies, KindSignatures, ConstraintKinds #-}

module BayesStack.Core( GatedPlate, gate
                      , Shared, newShared, setShared
                      , BayesStack.Core.sample
                      , Probability
		      , ProbDist(..)
                      , Sampleable(..)
                      ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
  
import Data.Random hiding (Sampleable)
import Data.IORef

import Control.Monad
import Control.Monad.IO.Class
  
import GHC.Prim (Constraint)

import qualified Data.Random.Distribution.Categorical as C

import BayesStack.ModelMonad
  
data GatedPlate control dist =
  GatedPlate (EnumMap control (Shared dist))

gate :: (Enum c) => GatedPlate c dist -> c -> Shared dist 
gate (GatedPlate e) c = e EM.! c

newtype Shared a = Shared (IORef a)

newShared :: a -> ModelMonad (Shared a)
newShared a = liftM Shared $ liftIO $ newIORef a

updateShared :: Shared a -> (a -> a) -> ModelMonad ()
updateShared (Shared a) f = liftIO $ atomicModifyIORef a (\x -> (f x, ()))
  
-- | Set a shared variable
infix 1 `setShared`
setShared :: Shared a -> a -> ModelMonad ()
(Shared a) `setShared` x = liftIO $ writeIORef a x
                           
getShared :: Shared a -> ModelMonad a
getShared (Shared a) = liftIO $ readIORef a

sample :: Sampleable unit => unit -> ModelMonad ()
sample unit = 
  do unset unit
     r <- range unit
     probs <- forM r $ sampleProb unit
     let dist = C.fromWeightedList $ zip probs r
     new <- Data.Random.sample dist
     set unit new
    
type Probability = Double

class Sampleable unit where
  type SValue unit :: *
  unset :: unit -> ModelMonad ()
  range :: unit -> ModelMonad [SValue unit]
  sampleProb :: unit -> SValue unit -> ModelMonad Probability
  set :: unit -> SValue unit -> ModelMonad ()

class ProbDist p where
  type PdContext p a :: Constraint
  type PdContext p a = ()
  prob :: PdContext p a => p a -> a -> Probability

