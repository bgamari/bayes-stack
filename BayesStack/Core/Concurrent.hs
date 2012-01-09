module BayesStack.Core.Concurrent ( concurrentRun
                                  , concurrentRunModel
                                  , chunk
                                  , numCapabilities
                                  ) where

import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.MVar
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
  where worker m sync = forkIO $ do m >>= putMVar sync

-- | Generate a new seed for MWC RNG
genSeed :: ModelMonad Seed
genSeed = liftM (toSeed . V.fromList) $ replicateM 256 $ Data.Random.sample stdUniform 

concurrentRunModel :: [ModelMonad ()] -> ModelMonad ()
concurrentRunModel as =
  do seeds <- replicateM numCapabilities genSeed 
     let runChunk (a,seed) = do mwc <- restore seed
                                runModel a mwc
     liftIO $ concurrentRun $ map runChunk $ zip as seeds
     return ()

