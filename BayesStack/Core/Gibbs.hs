{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts,
             ExistentialQuantification, GADTs #-}
              
module BayesStack.Core.Gibbs ( UpdateUnit(..)
                             , WrappedUpdateUnit(..)
                             , gibbsUpdate
                             ) where
                             
import Control.Monad (replicateM_, when, forever)
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef       
import Control.DeepSeq
import Data.Random
import Data.Random.Lift       
import System.Random.MWC (withSystemRandom)

class UpdateUnit uu where
    type ModelState uu
    type Setting uu
    fetchSetting   :: uu -> ModelState uu -> Setting uu
    evolveSetting  :: ModelState uu -> uu -> RVar (Setting uu)
    updateSetting  :: uu -> Setting uu -> Setting uu -> ModelState uu -> ModelState uu

data WrappedUpdateUnit ms = forall uu. (UpdateUnit uu, ModelState uu ~ ms, NFData (Setting uu))
                         => WrappedUU uu
     
updateUnit :: WrappedUpdateUnit ms -> IORef ms -> TBQueue (ms -> ms) -> RVarT IO ()
updateUnit (WrappedUU unit) stateRef diffQueue = do
    modelState <- lift $ readIORef stateRef
    let s = fetchSetting unit modelState
    s' <- lift $ evolveSetting modelState unit
    (s,s') `deepseq` return ()
    lift $ atomically $ writeTBQueue diffQueue (updateSetting unit s s')
    
updateWorker :: TQueue (WrappedUpdateUnit ms) -> IORef ms -> TBQueue (ms -> ms) -> RVarT IO ()
updateWorker unitsQueue stateRef diffQueue = do
    unit <- lift $ atomically $ tryReadTQueue unitsQueue
    case unit of
        Just unit' -> do updateUnit unit' stateRef diffQueue
                         updateWorker unitsQueue stateRef diffQueue
        Nothing -> return ()

diffWorker :: IORef ms -> TBQueue (ms -> ms) -> IO ()
diffWorker state diffQueue = forever $ do
    diff <- atomically $ readTBQueue diffQueue
    atomicModifyIORef' state $ \a->(diff a, ())

gibbsUpdate :: ms -> [WrappedUpdateUnit ms] -> IO ms
gibbsUpdate modelState units = do
    n <- getNumCapabilities
    unitsQueue <- atomically $ do q <- newTQueue
                                  mapM_ (writeTQueue q) units
                                  return q
    diffQueue <- atomically $ newTBQueue $ 100*n -- FIXME
    stateRef <- newIORef modelState
    diffThread <- forkIO $ diffWorker stateRef diffQueue

    runningWorkers <- atomically $ newTVar (0 :: Int)
    done <- atomically $ newEmptyTMVar :: IO (TMVar ())
    replicateM_ n $ forkIO $ withSystemRandom $ \mwc->do 
        atomically $ modifyTVar' runningWorkers (+1)
        runRVarT (updateWorker unitsQueue stateRef diffQueue) mwc
        atomically $ do
            modifyTVar' runningWorkers (+(-1))
            running <- readTVar runningWorkers
            when (running == 0) $ putTMVar done ()

    atomically $ takeTMVar done
    readIORef stateRef

