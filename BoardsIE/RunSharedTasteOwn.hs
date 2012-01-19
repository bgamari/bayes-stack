{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable #-}

import Data.Foldable

import Data.Number.LogFloat
import Data.Set (Set)
import qualified Data.Set as S
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.ByteString (ByteString)

import Control.Monad (when, liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Writer
import qualified Data.ByteString.Char8 as BS

import Data.Random hiding (gamma)
import System.Random.MWC (GenIO, withSystemRandom)

import Text.Printf
import Database.Redis
import Data.Serialize
import System.Console.CmdArgs hiding (args)

import BayesStack.Core
import BayesStack.UniqueKey
import BayesStack.Models.Topic.SharedTasteOwnSync

connectInfo = defaultConnectInfo {connectHost="blake"}

type NodeItemUniqueKey m = UniqueKeyT ByteString Node (UniqueKeyT ByteString Item m)

runNodeItemUniqueKey :: Monad m => NodeItemUniqueKey m a -> m a
runNodeItemUniqueKey = runUniqueKeyT Item . runUniqueKeyT Node

newNodeKey :: Monad m => ByteString -> NodeItemUniqueKey m Node
newNodeKey = newUniqueKey

newItemKey :: Monad m => ByteString -> NodeItemUniqueKey m Item
newItemKey = lift . newUniqueKey

serializeState :: STModel -> FilePath -> ModelMonad ()
serializeState model fname =
  do s <- getModelState model
     liftIO $ BS.writeFile fname $ runPut $ put s

data BoardsST = BoardsST { gamma_shared, gamma_own :: Double
                         , omega :: Double
                         , lambda :: Double
                         , psi :: Double
                         , phi :: Double
                         , topics :: Int
                         , sweeps_dir :: FilePath
                         , iterations :: Maybe Int
                         } deriving (Show, Data, Typeable)

boardsST = boardsST { gamma_shared = 45
                    , gamma_own = 5
                    , omega = 0.1
                    , psi = 0.5
                    , lambda = 0.1
                    , phi = 0.01
                    , topics = 10
                    , sweeps_dir = "sweeps"
                    , iterations = Just 100
                    }
--boardsST = boardsST { gamma_shared = 45 &= help "Alpha gamma"
--                    , gamma_own = 5 &= help "Alpha gamma"
--                    , omega = 0.1 &= help "Alpha omega"
--                    , psi = 0.5 &= help "Alpha psi"
--                    , lambda = 0.1 &= help "Alpha lambda"
--                    , phi = 0.01 &= help "Alpha phi"
--                    , topics = 10 &= help "Number of topics"
--                    , sweeps_dir = "sweeps" &= help "Directory to place sweep dumps in" &= opt "sweeps"
--                    , iterations = Just 100 &= help "Number of sweeps to run"
--                    }

main =
  do --args <- cmdArgs boardsST
     let args = boardsST
     conn <- connect $ connectInfo
     d <- runRedis conn $ runNodeItemUniqueKey $ do
       fs <- execWriterT getFriendships
       nodeItems <- execWriterT getNodeItems
       let nodes = foldMap (\(Friendship (u,f))->S.fromList [u,f]) fs
           items = foldMap (\(n,x)->S.singleton x) nodeItems
       return $ STData { stAlphaGammaShared = 0.1
                       , stAlphaGammaOwn = 0.1
                       , stAlphaOmega = 0.1
                       , stAlphaPsi = 0.1
                       , stAlphaLambda = 0.1
                       , stAlphaPhi = 0.01
                       , stNodes = nodes
                       , stFriendships = fs
                       , stItems = items
                       , stTopics = S.fromList $ map Topic [1..20]
                       , stNodeItems = setupNodeItems $ filter (\(n,x)->n `S.member` nodes) nodeItems
                       }
     printf "%d nodes, %d friendships, %d items, %d node items\n"
        (S.size $ stNodes d) (S.size $ stFriendships d) (S.size $ stItems d)
        (EM.size $ stNodeItems d)

     withSystemRandom $ runModel $ run args d

run args d =
  do liftIO $ putStr "Initializing model..."
     init <- liftRVar $ randomInitialize d
     (ius, m) <- model d init
     liftIO $ putStr $ printf "Created %d update units\n" (SQ.length ius)
     liftIO $ putStrLn "Starting inference"
     let gibbsUpdate :: Int -> S.StateT LogFloat ModelMonad ()
         gibbsUpdate sweepN =
           do l <- lift $ likelihood m
              lastMax <- S.get
              when (l > lastMax) $ do lift $ serializeState m $ printf "%s/%05d" (sweeps_dir args) sweepN
                                      S.put l
              liftIO $ putStr $ printf "Sweep %d: %f\n" sweepN (logFromLogFloat l :: Double)

              if sweepN > 20 then lift $ concurrentGibbsUpdate 10 ius
                             else lift $ concurrentFullGibbsUpdate 10 ius

     let nSweeps = maybe [0..] (\n->[0..n]) $ iterations args
     S.runStateT (forM_ nSweeps gibbsUpdate) 0


getFriendships :: WriterT (Set Friendship) (NodeItemUniqueKey Redis) ()
getFriendships = 
  do liftRedis $ sinterstore "%interestingPeople" ["%peopleWithFriends","%peopleWithKeywords"]
     Right people <- liftRedis $ smembers "%interestingPeople"
     forM_ people $ \p -> do
       u <- lift $ newNodeKey p
       Right friends <- liftRedis $ sinter [p `BS.append` "%knows", "%interestingPeople"]
       forM_ friends $ \p' -> do
         f <- lift $ newNodeKey p'
         tell $ S.singleton $ Friendship (u,f)
  where liftRedis = lift . lift . lift

getNodeItems :: WriterT [(Node,Item)] (NodeItemUniqueKey Redis) ()
getNodeItems = 
  do Right people <- liftRedis $ smembers "%interestingPeople"
     forM_ people $ \p -> do
       u <- lift $ newNodeKey p
       Right items <- liftRedis $ zrangebyscore (p `BS.append` "%keywords") 10 (100000)
       forM_ items $ \x -> do
         x' <- lift $ newItemKey x
         tell $ [(u,x')]
  where liftRedis = lift . lift . lift

