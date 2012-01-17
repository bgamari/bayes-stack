{-# LANGUAGE OverlappingInstances, DeriveDataTypeable #-}

import Prelude hiding (mapM)

import BayesStack.Core
import BayesStack.DirMulti
import BayesStack.Models.Topic.LDA
import LibThing.Data

import Data.List ((\\), nub, sort)

import Data.Set (Set)
import qualified Data.Set as S
  
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Map (Map)
import qualified Data.Map as M

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.Traversable (mapM)

import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import Control.Monad hiding (mapM)
  
import Data.Random
import System.Random.MWC (GenIO, withSystemRandom)

import System.IO

import Control.Concurrent

import qualified Data.ByteString as BS
import Data.Serialize

import Data.Number.LogFloat hiding (realToFrac)
import Text.Printf
  
import System.Console.CmdArgs

data LibThingLDA = LibThingLDA { theta :: Double
                               , phi :: Double
                               , topics :: Int
                               , sweeps_dir :: FilePath
                               , param_est :: Maybe Int
                               , param_est_holdoff :: Int
                               , iterations :: Maybe Int
                               } deriving (Show, Data, Typeable)

libThingLDA = LibThingLDA { theta = 0.1 &= help "Alpha theta"
                          , phi = 0.01 &= help "Alpha phi"
                          , topics = 10 &= help "Number of topics"
                          , sweeps_dir = "sweeps" &= typDir &= help "Directory to place sweep dumps in"
                          , param_est = Nothing &= help "Frequency with which to reestimate hyperparameters" &= opt (20::Int) &= typ "SWEEPS"
                          , param_est_holdoff = 20 &= help "Number of iterations before starting hyperparameter estimation" &= typ "SWEEPS"
                          , iterations = Just 100 &= help "Number of sweeps to run"
                          }


serializeState :: LDAModel -> FilePath -> ModelMonad ()
serializeState model fname =
  do s <- getModelState model
     liftIO $ BS.writeFile fname $ runPut $ put s

reestimateParams model =
  do alphas <- mapM getShared $ mThetas model
     let alphas' = reestimatePriors alphas
     mapM_ (\(k,theta)->setShared (mThetas model EM.! k) theta) $ EM.toList alphas'

     alphas <- mapM getShared $ mPhis model
     let alphas' = reestimateSymPriors alphas
     mapM_ (\(t,phi)->setShared (mPhis model EM.! t) phi) $ EM.toList alphas'

main = withSystemRandom $ runModel run
run = 
  do args <- liftIO $ cmdArgs libThingLDA
     (userTags, wordMap) <- liftIO readTags
     let d = LDAData { ldaAlphaTheta = theta args
                     , ldaAlphaPhi = phi args
                     , ldaNodes = S.fromList $ nub $ sort $ map fst userTags
                     , ldaItems = S.fromList $ nub $ sort $ map snd userTags
                     , ldaTopics = S.fromList $ map Topic [1..topics args]
                     , ldaNodeItems = setupNodeItems userTags
                     }
     liftIO $ putStrLn "Finished creating network"

     init <- liftRVar $ randomInitialize d
     (ius, model) <- model d init
     liftIO $ putStr $ printf "%d update units\n" (SQ.length ius)
  
     liftIO $ putStrLn "Starting inference"
     let gibbsUpdate :: Int -> S.StateT LogFloat ModelMonad ()
         gibbsUpdate sweepN =
           do l <- lift $ likelihood model
              lastMax <- S.get
              when (l > lastMax) $ do lift $ serializeState model $ printf "%s/%05d" (sweeps_dir args) sweepN
                                      S.put l
              liftIO $ putStr $ printf "Sweep %d: %f\n" sweepN (logFromLogFloat l :: Double)
              when (sweepN >= param_est_holdoff args
                 && maybe False (\n->sweepN `mod` n == 0) (param_est args)) $ do
                liftIO $ putStrLn "Parameter estimation"
                lift $ reestimateParams model
                lift $ concurrentGibbsUpdate 10 ius
                lift (likelihood model) >>= S.put
              lift $ concurrentGibbsUpdate 10 ius

     let nSweeps = maybe [0..] (\n->[0..n]) $ iterations args
     S.runStateT (forM_ nSweeps gibbsUpdate) 0
 
