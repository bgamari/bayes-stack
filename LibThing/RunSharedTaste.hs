{-# LANGUAGE OverlappingInstances, DeriveDataTypeable #-}

import Prelude hiding (mapM)

import BayesStack.Core
import BayesStack.Models.Topic.SharedTaste
import BayesStack.DirMulti
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
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
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

data LibThingST = LibThingST { psi :: Double
                             , lambda :: Double
                             , phi :: Double
                             , topics :: Int
                             , sweeps_dir :: FilePath
                             } deriving (Show, Data, Typeable)

libThingST = LibThingST { psi = 0.1 &= help "Alpha psi"
                        , lambda = 0.1 &= help "Alpha lambda"
                        , phi = 0.1 &= help "Alpha phi"
                        , topics = 10 &= help "Number of topics"
                        , sweeps_dir = "sweeps" &= help "Directory to place sweep dumps in" &= opt "sweeps"
                        }

serializeState :: STModel -> FilePath -> ModelMonad ()
serializeState model fname =
  do s <- getModelState model
     liftIO $ BS.writeFile fname $ runPut $ put s

reestimateParams model =
  do liftIO $ putStrLn "Lambda parameter reestimation"
     alphas <- mapM getShared $ mLambdas model
     let alphas' = reestimatePriors alphas
     mapM_ (\(u,lambda)->setShared (mLambdas model EM.! u) lambda) $ EM.toList alphas'
     liftIO $ print $ head $ EM.elems alphas'

     liftIO $ putStrLn "Phi parameter reestimation"
     alphas <- mapM getShared $ mPhis model
     let alphas' = reestimateSymPriors alphas
     mapM_ (\(t,phi)->setShared (mPhis model EM.! t) phi) $ EM.toList alphas'
     liftIO $ print $ head $ EM.elems alphas'

main = withSystemRandom $ runModel run
run = 
  do args <- liftIO $ cmdArgs libThingST
     (d', wordMap) <- liftIO getTags
     let d = d' { stAlphaPsi = psi args
                , stAlphaLambda = lambda args
                , stAlphaPhi = phi args
                , stTopics = S.fromList $ map Topic [1..topics args]
                }
     liftIO $ putStrLn "Finished creating network"
     (ius, model) <- model d
     liftIO $ putStr $ printf "%d update units\n" (SQ.length ius)

     liftIO $ BS.writeFile "word.map" $ runPut $ put wordMap
  
     liftIO $ putStrLn "Starting inference"
     let gibbsUpdate :: Int -> S.StateT LogFloat ModelMonad ()
         gibbsUpdate sweepN =
           do l <- lift $ likelihood model
              lastMax <- S.get
              when (l > lastMax) $ do lift $ serializeState model $ printf "%s/%05d" (sweeps_dir args) sweepN
                                      S.put l
              liftIO $ putStr $ printf "Sweep %d: %f\n" sweepN (logFromLogFloat l :: Double)
              when (sweepN >= 10 && sweepN `mod` 20 == 0) $ do lift $ reestimateParams model
                                                               lift (likelihood model) >>= S.put
              lift $ concurrentGibbsUpdate 10 ius

     S.runStateT (forM_ [0..] gibbsUpdate) 0
 
getTags :: IO (STData, Map Item String)
getTags =
  do (userTags, wordMap) <- readTags
     friendships <- readFriendships
     let d = STData { stAlphaPsi = 5.0
                    , stAlphaLambda = 0.1
                    , stAlphaPhi = 0.01
                    , stNodes = S.fromList $ nub $ sort $ map fst userTags
                    , stFriendships = S.fromList friendships
                    , stItems = S.fromList $ nub $ sort $ map snd userTags
                    , stTopics = S.empty
                    , stNodeItems = SQ.fromList userTags
                    }
     return (d, wordMap)

