{-# LANGUAGE OverlappingInstances #-}

import BayesStack.Core
import BayesStack.Models.Topic.SharedTasteOwn
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
  
import Data.Random
import System.Random.MWC (GenIO, withSystemRandom)

import System.IO

import Control.Concurrent

import qualified Data.ByteString as BS
import Data.Serialize

import Data.Number.LogFloat hiding (realToFrac)
import Text.Printf

topics = S.fromList $ map Topic [1..10]

serializeState :: STModel -> FilePath -> ModelMonad ()
serializeState model fname =
  do s <- getModelState model
     liftIO $ BS.writeFile fname $ runPut $ put s

main = withSystemRandom $ runModel run
run = 
  do (d, wordMap) <- liftIO getTags
     liftIO $ putStrLn "Finished creating network"
     (ius, model) <- model d
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
 
getTags :: IO (STData, Map Item String)
getTags =
  do friendships <- readFriendships
     (userTags, wordMap) <- readTags
     let d = STData { stAlphaGamma = [(True, 45.0), (False, 5.0)]
                    , stAlphaOmega = 1.0
                    , stAlphaPsi = 1.0
                    , stAlphaLambda = 0.1
                    , stAlphaPhi = 0.01
                    , stNodes = S.fromList $ nub $ sort $ map fst userTags
                    , stFriendships = S.fromList friendships
                    , stItems = S.fromList $ nub $ sort $ map snd userTags
                    , stTopics = topics
                    , stNodeItems = SQ.fromList userTags
                    }
     return (d, wordMap)

