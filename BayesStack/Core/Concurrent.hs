module BayesStack.Core.Concurrent ( concurrentRun
                                  , concurrentRunModels
                                  , chunk
                                  , numCapabilities
                                  ) where

import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent
import GHC.Conc (numCapabilities)

import Data.Random
import System.Random.MWC
import qualified Data.Vector as V

import BayesStack.Core.ModelMonad
import Data.Sequence.Chunk

-- | Run a list of 'IO' actions concurrently, collecting the results
-- Starts each action in a separate spark.
concurrentRun :: [IO a] -> IO [a]
concurrentRun ms =
  do syncs <- forM ms $ const newEmptyMVar
     forM (zip ms syncs) $ uncurry worker
     forM syncs takeMVar
  where worker m sync = forkIO $ m >>= putMVar sync

-- | Generate a new seed for MWC RNG
genSeed :: ModelMonad Seed
genSeed = liftM (toSeed . V.fromList) $ replicateM 256 $ Data.Random.sample stdUniform 

concurrentRunModels :: [ModelMonad ()] -> ModelMonad ()
concurrentRunModels chunks =
  do seeds <- replicateM (length chunks) genSeed 
     let runChunk chunk seed = do mwc <- restore seed
                                  runModel chunk mwc
     liftIO $ concurrentRun $ zipWith runChunk chunks seeds
     return ()

