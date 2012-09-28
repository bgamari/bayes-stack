{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts,
             ExistentialQuantification, GADTs #-}
              
module BayesStack.Core.Gibbs ( UpdateUnit(..)
                             , WrappedUpdateUnit(..)
                             , gibbsUpdate
                             ) where
                             
import Control.Monad (replicateM_, when)
import Control.Concurrent
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
     
updateUnit :: MVar ms -> WrappedUpdateUnit ms -> RVarT IO ()
updateUnit modelStateV (WrappedUU unit) = do
    modelState <- lift $ readMVar modelStateV
    let s = fetchSetting unit modelState
    s' <- lift $ evolveSetting modelState unit
    deepseq (s, s') $ return ()
    lift $ modifyMVar_ modelStateV (return . updateSetting unit s s')
    
maybeHead :: [a] -> ([a], Maybe a)
maybeHead [] = ([], Nothing)
maybeHead (a:rest) = (rest, Just a)

updateWorker :: MVar ms -> MVar [WrappedUpdateUnit ms] -> RVarT IO ()
updateWorker modelStateV unitsV = do
    units <- lift $ modifyMVar unitsV $ return . maybeHead
    case units of
        Just unit -> do updateUnit modelStateV unit
                        updateWorker modelStateV unitsV
        Nothing -> return ()

gibbsUpdate :: ms -> [WrappedUpdateUnit ms] -> IO ms
gibbsUpdate modelState units = do
    unitsV <- newMVar units
    modelStateV <- newMVar modelState
    n <- getNumCapabilities
    runningWorkers <- newMVar (0 :: Int)
    done <- newEmptyMVar :: IO (MVar ())
    replicateM_ n $ forkIO $ withSystemRandom $ \mwc->do 
        modifyMVar_ runningWorkers $ \n->return $ n+1
        runRVarT (updateWorker modelStateV unitsV) mwc
        modifyMVar_ runningWorkers $ \n->return $ n-1
        running <- readMVar runningWorkers
        when (running == 0) $ putMVar done ()

    takeMVar done
    modelState' <- takeMVar modelStateV
    return modelState'

