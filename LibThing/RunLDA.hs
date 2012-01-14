{-# LANGUAGE OverlappingInstances, DeriveDataTypeable #-}

import BayesStack.Core
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

import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
import Control.Monad
  
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
                               } deriving (Show, Data, Typeable)

libThingLDA = LibThingLDA { theta = 0.1 &= help "Alpha theta"
                          , phi = 0.1 &= help "Alpha phi"
                          , topics = 10 &= help "Number of topics"
                          , sweeps_dir = "sweeps" &= help "Directory to place sweep dumps in" &= opt "sweeps"
                          }


serializeState :: LDAModel -> FilePath -> ModelMonad ()
serializeState model fname =
  do s <- getModelState model
     liftIO $ BS.writeFile fname $ runPut $ put s

main = withSystemRandom $ runModel run
run = 
  do args <- liftIO $ cmdArgs libThingLDA
     (d', wordMap) <- liftIO getTags
     let d = d' { ldaAlphaTheta = theta args
                , ldaAlphaPhi = phi args
                , ldaTopics = S.fromList $ map Topic [1..topics args]
                }
     liftIO $ putStrLn "Finished creating network"
     (ius, model) <- model d
  
     liftIO $ putStrLn "Starting inference"
     let gibbsUpdate :: Int -> S.StateT LogFloat ModelMonad ()
         gibbsUpdate sweepN =
           do l <- lift $ likelihood model
              lastMax <- S.get
              --when (l > lastMax) $ do lift $ serializeState model $ printf "sweeps/%05d" sweepN
              --                        S.put l
              liftIO $ putStr $ printf "Sweep %d: %f\n" sweepN (logFromLogFloat l :: Double)
              lift $ concurrentGibbsUpdate 10 ius

     S.runStateT (forM_ [0..] gibbsUpdate) 0
 
getTags :: IO (LDAData, Map Item String)
getTags =
  do (userTags, wordMap) <- readTags
     let d = LDAData { ldaAlphaTheta = 0.1
                     , ldaAlphaPhi = 0.1
                     , ldaNodes = S.fromList $ nub $ sort $ map fst userTags
                     , ldaItems = S.fromList $ nub $ sort $ map snd userTags
                     , ldaTopics = S.empty
                     , ldaNodeItems = SQ.fromList userTags
                     }
     return (d, wordMap)

