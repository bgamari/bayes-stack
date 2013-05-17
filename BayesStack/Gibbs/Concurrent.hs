{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, GADTs, CPP #-}

module BayesStack.Gibbs.Concurrent ( gibbsUpdate
                                   , module BayesStack.Gibbs
                                   ) where

import BayesStack.Gibbs
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad (replicateM_, when, forever)
import Control.Monad.State hiding (lift)
import Data.IORef
import Data.Random
import Data.Random.Lift
import Debug.Trace (traceEventIO)
import GHC.Conc.Sync (labelThread)
import System.Random.MWC (withSystemRandom)

updateUnit :: WrappedUpdateUnit ms -> IORef ms -> TBQueue (ms -> ms) -> RVarT IO ()
updateUnit (WrappedUU unit) stateRef diffQueue = do
    modelState <- lift $ readIORef stateRef
    let s = fetchSetting unit modelState
    s' <- lift $ evolveSetting modelState unit
    (s,s') `deepseq` return ()
    liftIO $ print (unit, s, s')
    when (s /= s') $
        lift $ atomically $ writeTBQueue diffQueue (updateSetting unit s s')

updateWorker :: TQueue (WrappedUpdateUnit ms) -> IORef ms -> TBQueue (ms -> ms) -> RVarT IO ()
updateWorker unitsQueue stateRef diffQueue = do
    unit <- lift $ atomically $ tryReadTQueue unitsQueue
    case unit of
        Just unit' -> do updateUnit unit' stateRef diffQueue
                         updateWorker unitsQueue stateRef diffQueue
        Nothing -> return ()

#if __GLASGOW_HASKELL__ < 706
atomicModifyIORef' = atomicModifyIORef
#endif

diffWorker :: IORef ms -> TBQueue (ms -> ms) -> Int -> IO ()
diffWorker stateRef diffQueue updateBlock = forever $ do
    s <- readIORef stateRef
    s' <- execStateT (replicateM_ updateBlock $ do
                          diff <- lift $ atomically $ readTBQueue diffQueue
                          modify diff
                     ) s
    atomicWriteIORef stateRef $! s'
    traceEventIO "diffWorker: State updated"

labelMyThread :: String -> IO ()
labelMyThread label = myThreadId >>= \id->labelThread id label

gibbsUpdate :: Int -> Int -> ms -> [WrappedUpdateUnit ms] -> IO ms
gibbsUpdate nUpdateWorkers updateBlock modelState units = do
    unitsQueue <- atomically $ do q <- newTQueue
                                  mapM_ (writeTQueue q) units
                                  return q
    diffQueue <- atomically $ newTBQueue $ 2*updateBlock -- FIXME
    stateRef <- newIORef modelState
    diffThread <- forkIO $ do labelMyThread "diff worker"
                              diffWorker stateRef diffQueue updateBlock

    runningWorkers <- atomically $ newTVar (0 :: Int)
    done <- atomically $ newEmptyTMVar :: IO (TMVar ())
    replicateM_ nUpdateWorkers $ forkIO $ withSystemRandom $ \mwc->do
        labelMyThread "update worker"
        atomically $ modifyTVar' runningWorkers (+1)
        runRVarT (updateWorker unitsQueue stateRef diffQueue) mwc
        atomically $ do
            modifyTVar' runningWorkers (+(-1))
            running <- readTVar runningWorkers
            when (running == 0) $ putTMVar done ()

    atomically $ takeTMVar done
    readIORef stateRef