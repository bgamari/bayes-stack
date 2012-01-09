{-# LANGUAGE OverlappingInstances #-}

import BayesStack.Core
import BayesStack.UniqueKey
import LDA

import Data.List ((\\))

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
import Control.Monad
  
import Control.Arrow (second)
import Data.List
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Function (on)
import Data.Char (toLower)
  
import Data.Random
import System.Random.MWC (GenIO, withSystemRandom)

import System.IO

import Control.Concurrent
import Control.Concurrent.MVar

import Data.Number.LogFloat hiding (realToFrac)
import Text.Printf
  
import Data.Hashable
import Text.CSV

topics = S.fromList $ map Topic [1..10]

main = withSystemRandom $ runModel run
run = 
  do let topics = [Topic i | i <- [0..10]]
     (d, wordMap) <- liftIO getTags
     liftIO $ putStrLn "Finished creating network"
     (ius, vars) <- model d
     --liftIO $ printf "%d update units\n" (SQ.length ius)
  
     liftIO $ putStrLn "Starting inference"
     f <- liftIO $ openFile "log" WriteMode
     forM_ [1..] $ \i ->
       do l <- likelihood vars
          liftIO $ putStr $ printf "Sweep %d: %f\n" (i::Int) (logFromLogFloat l :: Double)
          liftIO $ hPutStr f $ printf "%d\t%f\n" (i::Int) (logFromLogFloat l :: Double)
          liftIO $ hFlush f
          --forM (take 3 $ EM.toList $ mThetas vars ) $ \(c,d) -> do d' <- getShared d
          --                                                         liftIO $ print c
          --                                                         liftIO $ print d'
          concurrentGibbsUpdate (SQ.length ius) ius
     liftIO $ hClose f
     liftIO $ putStrLn "Finished"
 
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads 

getTags :: IO (LDAData, Map Item String)
getTags =
  do f <- parseCSVFromFile "tags_for_users.csv"
     let csv = either (error . show) id f
         records = mapMaybe (\rec -> do a <- maybeRead $ rec !! 0
                                        return (a, map toLower $ rec !! 6))
                   $ tail csv
         (userTags, wordMap) = runUniqueKey $ forM records $ \(n,i) -> do k <- uniqueKey Item i
                                                                          return (Node n, k)
     let d = LDAData { ldaAlphaTheta = 0.1
                     , ldaAlphaPhi = 0.1
                     , ldaNodes = S.fromList $ nub $ sort $ map fst userTags
                     , ldaItems = S.fromList $ nub $ sort $ map snd userTags
                     , ldaTopics = topics
                     , ldaNodeItems = SQ.fromList userTags
                     }
     return (d, wordMap)

