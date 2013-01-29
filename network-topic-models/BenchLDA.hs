module BenchLDA where

import           Control.Monad.Trans.State
import           Control.Monad (replicateM, forM)
import           Control.Applicative ((<$>))
import           Text.Printf
import           Control.Concurrent (setNumCapabilities)

import           Criterion
import           Data.Random

import qualified Data.Set as S
import           BayesStack.Core.Gibbs
import           BayesStack.Models.Topic.LDA

data NetParams = NetParams { nNodes        :: Int
                           , nItems        :: Int
                           , nTopics       :: Int
                           , nItemsPerNode :: Int
                           }
               deriving (Show, Eq, Ord)

netParams = NetParams { nNodes = 50000
                      , nItems = nItemsPerNode netParams * nNodes netParams `div` 10
                      , nTopics = 100
                      , nItemsPerNode = 200
                      }

randomNetwork :: NetParams -> RVar NetData
randomNetwork net = do
    let nodes = [Node i | i <- [1..nNodes net]]
        items = [Item i | i <- [1..nItems net]]
        nodeItem = do node <- randomElement nodes
                      item <- randomElement items
                      return (node, item)
    edges <- replicateM (nItemsPerNode net) nodeItem
    return $! NetData { dAlphaTheta = 0.1
                      , dAlphaPhi   = 0.1
                      , dNodes      = S.fromList nodes
                      , dItems      = S.fromList items
                      , dTopics     = S.fromList [Topic i | i <- [1..nTopics net]]
                      , dNodeItems  = setupNodeItems edges
                      }

benchmarksForNetwork :: NetParams -> NetData -> ModelInit -> [Benchmark]
benchmarksForNetwork np net init = do
    let sweeps = 100
    updateBlock <- [10, 100, 1000]
    threads <- [1, 2, 3, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26]
    let name = printf "%d topics, %d threads, %d block, %d items per node" (nTopics np) threads updateBlock (nItemsPerNode np)
    return $ bench name $ do
        setNumCapabilities threads
        gibbsUpdate threads updateBlock (model net init)
            $ concat $ replicate sweeps (updateUnits net)

benchmarksForNetParams :: NetParams -> RVar [Benchmark]
benchmarksForNetParams np = do
    net <- randomNetwork np
    init <- randomInitialize net
    return $ benchmarksForNetwork np net init

ldaBenchmarkParams :: RVar [[Benchmark]]
ldaBenchmarkParams =
    mapM benchmarksForNetParams
    $ do topics <- [100, 500, 1000]
         return netParams {nTopics=topics}

ldaBenchmarks :: RVar Benchmark
ldaBenchmarks = bgroup "LDA" . concat <$> ldaBenchmarkParams 

