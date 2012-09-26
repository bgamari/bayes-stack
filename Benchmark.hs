{-# LANGUAGE FlexibleContexts #-}                

import Control.DeepSeq                
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Control.Monad (replicateM, forM)
import qualified Data.Set as S                

import Criterion
import Criterion.Config
import Criterion.Main

import Data.Random
import System.Random.MWC       

import BayesStack.Core.Gibbs                
import BayesStack.Models.Topic.LDA

data NetParams = NetParams { nNodes        :: Int
                           , nItems        :: Int
                           , nTopics       :: Int
                           , nItemsPerNode :: Int
                           }

netParams = NetParams { nNodes = 2000, nItems = 30000, nTopics = 100, nItemsPerNode = 20 }

randomNetwork :: NetParams -> RVar LDAData
randomNetwork net = do
    let nodes = [Node i | i <- [1..nNodes net]]
        items = [Item i | i <- [1..nItems net]]
        nodeItem = do node <- randomElement nodes
                      item <- randomElement items
                      return (node, item)
    edges <- replicateM (nItemsPerNode net) nodeItem
    return $ LDAData { ldaAlphaTheta = 0.1
                     , ldaAlphaPhi   = 0.1
                     , ldaNodes      = S.fromList nodes
                     , ldaItems      = S.fromList items
                     , ldaTopics     = S.fromList [Topic i | i <- [1..nTopics net]]
                     , ldaNodeItems  = setupNodeItems edges
                     }

withSystemRandomIO :: (GenIO -> IO a) -> IO a          
withSystemRandomIO = withSystemRandom                   

benchmarks :: [(String, NetParams)] -> IO [Benchmark]
benchmarks xs = withSystemRandomIO $ runRVar (forM xs f) :: IO [Benchmark]
    where f :: (String, NetParams) -> RVar Benchmark
          f (name, params) = do
              net <- randomNetwork params
              init <- randomInitialize net
              return $ bench name $ gibbsUpdate (model net init) (updateUnits net)
    
main = do
    ldaBenchmarks <- benchmarks
        [ ("100 topics", netParams {nTopics=100})
        , ("10 topics", netParams {nTopics=10})
        ]

    defaultMainWith defaultConfig (return ())
        [ bgroup "LDA" ldaBenchmarks
        ]