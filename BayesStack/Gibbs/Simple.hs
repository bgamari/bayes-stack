{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, GADTs, CPP #-}

module BayesStack.Gibbs.Simple ( gibbsUpdate
                               , module BayesStack.Gibbs
                               ) where

import BayesStack.Gibbs
import Control.Monad.State hiding (lift)
import Control.DeepSeq       
import Data.Random
import Data.Random.Lift
import System.Random.MWC       

updateUnit :: WrappedUpdateUnit ms -> StateT ms RVar ()
updateUnit (WrappedUU unit) = do
    ms <- get
    let s = fetchSetting unit ms
    s' <- lift $ evolveSetting ms unit
    (s,s') `deepseq` return ()
    put $ updateSetting unit s s' ms

gibbsUpdate :: ms -> [WrappedUpdateUnit ms] -> IO ms
gibbsUpdate modelState units = withSystemRandom $ asGenIO $ \mwc->
    runRVar (execStateT (mapM_ updateUnit units) modelState) mwc