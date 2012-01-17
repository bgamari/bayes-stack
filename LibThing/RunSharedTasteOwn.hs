{-# LANGUAGE OverlappingInstances, DeriveDataTypeable #-}

import Prelude hiding (mapM)

import BayesStack.Core
import BayesStack.DirMulti
--import BayesStack.Models.Topic.SharedTasteOwn
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

import Data.Traversable (mapM)
import Data.Foldable (forM_)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Control.Monad hiding (mapM, forM_)

import Data.Random hiding (gamma)
import System.Random.MWC (GenIO, withSystemRandom)

import System.IO

import Control.Concurrent

import qualified Data.ByteString as BS
import Data.Serialize

import Data.Number.LogFloat hiding (realToFrac)
import Text.Printf

import System.Console.CmdArgs

data LibThingST = LibThingST { gamma_shared, gamma_own :: Double
                             , omega :: Double
                             , lambda :: Double
                             , psi :: Double
                             , phi :: Double
                             , topics :: Int
                             , sweeps_dir :: FilePath
                             , param_est :: Maybe Int
                             , param_est_holdoff :: Int
                             , own_holdoff :: Maybe Int
                             , iterations :: Maybe Int
                             } deriving (Show, Data, Typeable)

libThingST = LibThingST { gamma_shared = 45 &= help "Alpha gamma"
                        , gamma_own = 5 &= help "Alpha gamma"
                        , omega = 0.1 &= help "Alpha omega"
                        , psi = 0.5 &= help "Alpha psi"
                        , lambda = 0.1 &= help "Alpha lambda"
                        , phi = 0.01 &= help "Alpha phi"
                        , topics = 10 &= help "Number of topics"
                        , sweeps_dir = "sweeps" &= help "Directory to place sweep dumps in" &= opt "sweeps"
                        , param_est = Nothing &= help "Frequency with which to reestimate hyperparameters" &= opt (20::Int) &= typ "SWEEPS"
                        , param_est_holdoff = 20 &= help "Number of iterations to hold-off hyperparameter estimation" &= typ "SWEEPS"
                        , own_holdoff = Nothing &= help "Number of iterations to hold-off enabling own items" &= typ "SWEEPS" &= opt (0::Int)
                        , iterations = Just 100 &= help "Number of sweeps to run"
                        }

serializeState :: STModel -> FilePath -> ModelMonad ()
serializeState model fname =
  do s <- getModelState model
     liftIO $ BS.writeFile fname $ runPut $ put s

reestimateGamma model =
  do dms <- mapM getShared $ mGammas model
     let dms' = reestimatePriors dms
     mapM_ (\(u,lambda)->setShared (mGammas model EM.! u) lambda) $ EM.toList dms'
     liftIO $ putStr "Alpha Gammas"
     liftIO $ print $ prettyDirMulti 10 show $ head $ EM.elems dms'

reestimateOmega model =
  do dms <- mapM getShared $ mOmegas model
     liftIO $ mapM_ (print) $ EM.toList dms
     let dms' = reestimatePriors dms
     mapM_ (\(k,v)->setShared (mOmegas model EM.! k) v) $ EM.toList dms'
     liftIO $ putStr "Alpha Omegas"
     liftIO $ print $ prettyDirMulti 10 show $ head $ EM.elems dms'

reestimatePsi model =
  do dms <- mapM getShared $ mPsis model
     let dms' = reestimatePriors dms
     mapM_ (\(u,psi)->setShared (mPsis model EM.! u) psi) $ EM.toList dms'
     liftIO $ putStr "Alpha Psi"
     liftIO $ print $ prettyDirMulti 10 show $ head $ EM.elems dms'

reestimateLambda model =
  do dms <- mapM getShared $ mLambdas model
     let dms' = reestimatePriors dms
     mapM_ (\(u,lambda)->setShared (mLambdas model EM.! u) lambda) $ EM.toList dms'
     liftIO $ putStr "Alpha Lambda"
     liftIO $ print $ prettyDirMulti 10 show $ head $ EM.elems dms'

reestimatePhi model =
  do dms <- mapM getShared $ mPhis model
     let dms' = reestimateSymPriors dms
     mapM_ (\(t,phi)->setShared (mPhis model EM.! t) phi) $ EM.toList dms'
     liftIO $ putStr "Alpha Phi"
     liftIO $ print $ prettyDirMulti 10 show $ head $ EM.elems dms'

main = withSystemRandom $ runModel run
run = 
  do args <- liftIO $ cmdArgs libThingST
     (userTags, wordMap) <- liftIO readTags
     liftIO $ BS.writeFile "word.map" $ runPut $ put wordMap
     friendships <- liftIO readFriendships
     let d = STData { stAlphaGammaShared = gamma_shared args
                    , stAlphaGammaOwn = 0
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

     init <- liftRVar $ smartInitialize d
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
              c <- lift $ mapM getShared $ EM.elems $ mOmegas model
              liftIO $ putStrLn $ show $ sum $ map dmTotal c
              c <- lift $ mapM getShared $ EM.elems $ mLambdas model
              liftIO $ putStrLn $ show $ sum $ map dmTotal c

              when (sweepN >= param_est_holdoff args
                 && maybe False (\n->sweepN `mod` n == 0) (param_est args)) $ do
                liftIO $ putStrLn "Parameter estimation"
                lift $ reestimateLambda model
                lift $ reestimatePhi model
                lift $ concurrentGibbsUpdate 10 ius
                lift (likelihood model) >>= S.put

              when (maybe False (sweepN==) (own_holdoff args)) $ do
                let update = updatePrior $ setAlphaOf Own (gamma_own args)
                lift $ forM_ (mGammas model) $ \dm->updateShared dm update
                liftIO $ putStrLn "Enabled own topics"

              lift $ concurrentGibbsUpdate 10 ius

     let nSweeps = maybe [0..] (\n->[0..n]) $ iterations args
     S.runStateT (forM_ nSweeps gibbsUpdate) 0

