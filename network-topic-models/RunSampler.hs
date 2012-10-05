module RunSampler ( SamplerModel (..)
                  , SamplerOpts (..), samplerOpts
                  , runSampler
                  ) where
                  
import           Options.Applicative    
import           Data.Monoid ((<>))                 
import           System.FilePath.Posix ((</>))
import           System.Directory (createDirectoryIfMissing)

import           Control.Monad (when, forM_, void)
import qualified Control.Monad.Trans.State as S
import           Control.Monad.IO.Class

import           Data.Serialize
import qualified Data.ByteString as BS
import           Text.Printf

import           Control.Concurrent
import           Control.Concurrent.STM       

import           System.Random.MWC                 
import           Data.Random
                 
import           Data.Number.LogFloat (LogFloat, logFromLogFloat)
import           BayesStack.Core

data SamplerOpts = SamplerOpts { burnin          :: Int
                               , lag             :: Int
                               , iterations      :: Maybe Int
                               , updateBlock     :: Int
                               , sweepsDir       :: FilePath
                               , nCaps           :: Int
                               , hyperEstOpts    :: HyperEstOpts
                               }

samplerOpts = SamplerOpts
    <$> option     ( long "burnin"
                  <> short 'b'
                  <> metavar "N"
                  <> value 1000
                  <> help "Number of sweeps to run before taking samples"
                   )
    <*> option     ( long "lag"
                  <> short 'l'
                  <> metavar "N"
                  <> value 10
                  <> help "Number of sweeps between diagnostic samples"
                   )
    <*> option     ( long "iterations"
                  <> short 'i'
                  <> metavar "N"
                  <> value Nothing
                  <> reader (Just . auto)
                  <> help "Number of sweeps to run for"
                   )
    <*> option     ( long "update-block"
                  <> short 'u'
                  <> metavar "N"
                  <> value 100
                  <> help "Number of update diffs to batch before updating global state"
                   )
    <*> strOption  ( long "sweeps"
                  <> short 'S'
                  <> metavar "DIR"
                  <> value "sweeps"
                  <> help "Directory in which to place model state output"
                   )
    <*> option     ( long "threads"
                  <> short 'N'
                  <> metavar "INT"
                  <> help "Number of worker threads to start"
                   )
    <*> hyperEstOpts'

data HyperEstOpts = HyperEstOpts { hyperEst       :: Bool
                                 , hyperBurnin    :: Int
                                 , hyperLag       :: Int
                                 }

hyperEstOpts' = HyperEstOpts
    <$> switch     ( long "hyper"
                  <> short 'h'
                  <> help "Enable hyperparameter estimation"
                   )
    <*> option     ( long "hyper-burnin"
                  <> metavar "N"
                  <> value 10
                  <> help "Number of sweeps before starting hyperparameter estimations (must be multiple of --lag)"
                   )
    <*> option     ( long "hyper-lag"
                  <> metavar "L"
                  <> value 10
                  <> help "Number of sweeps between hyperparameter estimations (must be multiple of --lag)"
                   )

class Serialize ms => SamplerModel ms where
    modelLikelihood :: ms -> LogFloat
    estimateHypers :: ms -> ms
    summarizeHypers :: ms -> String
    
serializeState :: Serialize ms => ms -> FilePath -> IO ()
serializeState model fname = BS.writeFile fname $ runPut $ put model

processSweep :: SamplerModel ms => SamplerOpts -> TVar LogFloat -> Int -> ms -> IO ()
processSweep opts lastMaxV sweepN m = do             
    let l = modelLikelihood m
    putStr $ printf "Sweep %d: %f\n" sweepN (logFromLogFloat l :: Double)
    appendFile "likelihood.txt" $ printf "%d\t%f\n" sweepN (logFromLogFloat l :: Double)
    newMax <- atomically $ do oldL <- readTVar lastMaxV
                              if l > oldL then writeTVar lastMaxV l >> return True
                                          else return False
    when newMax
        $ serializeState m $ sweepsDir opts </> printf "%05d.state" sweepN

doEstimateHypers :: SamplerModel ms => HyperEstOpts -> Int -> S.StateT ms IO ()
doEstimateHypers (HyperEstOpts True burnin lag) iterN
    | iterN >= burnin && iterN `mod` lag == 0  = do
        liftIO $ putStrLn "Parameter estimation"
        m <- S.get
        S.modify estimateHypers
        m' <- S.get
        void $ liftIO $ forkIO
            $ appendFile "hyperparams.log"
            $ printf "%5d\t%f\t%f\t%s\n"
                  iterN
                  (logFromLogFloat $ modelLikelihood m :: Double)
                  (logFromLogFloat $ modelLikelihood m' :: Double)
                  (summarizeHypers m')
doEstimateHypers _ _  = return ()

withSystemRandomIO = withSystemRandom :: (GenIO -> IO a) -> IO a

samplerIter :: SamplerModel ms => SamplerOpts -> [WrappedUpdateUnit ms] -> TVar LogFloat
            -> Int -> S.StateT ms IO ()
samplerIter opts uus lastMaxV lagN = do
    let sweepN = lagN * lag opts
    m <- S.get
    shuffledUus <- liftIO $ withSystemRandomIO $ \mwc->runRVar (shuffle uus) mwc
    let uus' = concat $ replicate (lag opts) shuffledUus
    S.put =<< liftIO (gibbsUpdate (updateBlock opts) m uus')
    when (sweepN == burnin opts) $ liftIO $ putStrLn "Burn-in complete"
    when (sweepN >= burnin opts) $ 
        S.get >>= void . liftIO . forkIO . processSweep opts lastMaxV sweepN
    doEstimateHypers (hyperEstOpts opts) sweepN

checkOpts :: SamplerOpts -> IO ()
checkOpts opts = do
    let hyperOpts = hyperEstOpts opts
    when (burnin opts `mod` lag opts /= 0)
        $ error "--burnin must be multiple of --lag"
    when (hyperEst hyperOpts && hyperBurnin hyperOpts `mod` lag opts /= 0)
        $ error "--hyper-burnin must be multiple of --lag"
    when (hyperEst hyperOpts && hyperLag hyperOpts `mod` lag opts /= 0)
        $ error "--hyper-lag must be multiple of --lag"

runSampler :: SamplerModel ms => SamplerOpts -> ms -> [WrappedUpdateUnit ms] -> IO ()
runSampler opts m uus = do
    checkOpts opts
    setNumCapabilities (nCaps opts)
    createDirectoryIfMissing False (sweepsDir opts)
    putStrLn "Starting sampler..."
    putStrLn $ "Burning in for "++show (burnin opts)++" samples"
    let lagNs = maybe [0..] (\n->[0..n]) $ iterations opts           
    lastMaxV <- atomically $ newTVar 0
    void $ S.runStateT (forM_ lagNs (samplerIter opts uus lastMaxV)) m

