{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}
              
module BayesStack.Core.Gibbs ( UpdateUnit(..)
                             , gibbsUpdate
                             ) where
                             
import Control.Monad (replicateM, when)
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
 
maybeHead :: [a] -> ([a], Maybe a)
maybeHead [] = ([], Nothing)
maybeHead (head:rest) = (rest, Just head)

updateWorker :: (UpdateUnit uu, NFData (Setting uu))
             => MVar (ModelState uu) -> MVar [uu] -> RVarT IO ()
updateWorker modelStateV unitsV = do
    units <- lift $ modifyMVar unitsV $ return . maybeHead
    case units of
        Just unit -> do
            modelState <- lift $ readMVar modelStateV
            let s = fetchSetting unit modelState
            s' <- lift $ evolveSetting modelState unit
            deepseq (s, s') $ return ()
            lift $ modifyMVar_ modelStateV (return . updateSetting unit s s')
            updateWorker modelStateV unitsV
        Nothing -> return ()

gibbsUpdate :: (UpdateUnit uu, NFData (Setting uu))
       => ModelState uu -> [uu] -> IO (ModelState uu)
gibbsUpdate modelState units = do
    unitsV <- newMVar units
    modelStateV <- newMVar modelState
    n <- getNumCapabilities
    runningWorkers <- newMVar (0 :: Int)
    done <- newEmptyMVar :: IO (MVar ())
    replicateM n $ forkIO $ withSystemRandom $ \mwc->do 
        modifyMVar_ runningWorkers $ \n->return $ n+1
        runRVarT (updateWorker modelStateV unitsV) mwc
        modifyMVar_ runningWorkers $ \n->return $ n-1
        running <- readMVar runningWorkers
        when (running == 0) $ putMVar done ()

    takeMVar done
    modelState' <- takeMVar modelStateV
    return modelState'

