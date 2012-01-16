{-# LANGUAGE OverlappingInstances, DeriveDataTypeable #-}

import BayesStack.Core
import BayesStack.Models.Topic.SharedTasteOwnSync
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
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Control.Monad

import Data.Random hiding (gamma)
import System.Random.MWC (GenIO, withSystemRandom)

import System.IO

import Control.Concurrent

import qualified Data.ByteString as BS
import Data.Serialize

import Data.Number.LogFloat hiding (realToFrac)
import Text.Printf

import System.Console.CmdArgs

data LibThingST = LibThingST { gamma :: Double
                             , omega :: Double
                             , lambda :: Double
                             , psi :: Double
                             , phi :: Double
                             , topics :: Int
                             , sweeps_dir :: FilePath
                             } deriving (Show, Data, Typeable)

libThingST = LibThingST { gamma = 0.1 &= help "Alpha gamma"
                        , omega = 0.1 &= help "Alpha omega"
                        , psi = 0.1 &= help "Alpha psi"
                        , lambda = 0.1 &= help "Alpha lambda"
                        , phi = 0.1 &= help "Alpha phi"
                        , topics = 10 &= help "Number of topics"
                        , sweeps_dir = "sweeps" &= help "Directory to place sweep dumps in" &= opt "sweeps"
                        }

serializeState :: STModel -> FilePath -> ModelMonad ()
serializeState model fname =
  do s <- getModelState model
     liftIO $ BS.writeFile fname $ runPut $ put s

main = withSystemRandom $ runModel run
run = 
  do args <- liftIO $ cmdArgs libThingST
     (userTags, wordMap) <- liftIO readTags
     friendships <- liftIO readFriendships
     let d = STData { stAlphaGamma = [(True, gamma args), (False, 1-gamma args)]
                    , stAlphaOmega = omega args
                    , stAlphaPsi = psi args
                    , stAlphaLambda = lambda args
                    , stAlphaPhi = phi args
                    , stNodes = S.fromList $ nub $ sort $ map fst userTags
                    , stFriendships = S.fromList friendships
                    , stItems = S.fromList $ nub $ sort $ map snd userTags
                    , stTopics = S.fromList $ map Topic [1..topics args]
                    , stNodeItems = setupNodeItems userTags
                    }
     liftIO $ putStrLn "Finished creating network"

     init <- liftRVar $ randomInitialize d
     (ius, model) <- model d init
     liftIO $ putStr $ printf "%d update units\n" (SQ.length ius)

     liftIO $ BS.writeFile "word.map" $ runPut $ put wordMap
  
     liftIO $ putStrLn "Starting inference"
     let gibbsUpdate :: Int -> S.StateT LogFloat ModelMonad ()
         gibbsUpdate sweepN =
           do l <- lift $ likelihood model
              lastMax <- S.get
              when (l > lastMax) $ do lift $ serializeState model $ printf "sweeps/%05d" sweepN
                                      S.put l
              liftIO $ putStr $ printf "Sweep %d: %f\n" sweepN (logFromLogFloat l :: Double)
              lift $ concurrentGibbsUpdate 10 ius

     S.runStateT (forM_ [0..] gibbsUpdate) 0
 
