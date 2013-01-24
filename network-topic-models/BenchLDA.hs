module BenchLDA where

import           Control.Monad.Trans.State
import           Control.Monad (replicateM, forM)
import           Text.Printf

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

netParams = NetParams { nNodes = 5000
                      , nItems = nItemsPerNode netParams * nNodes netParams `div` 10
                      , nTopics = 100
                      , nItemsPerNode = 20
                      }

randomNetwork :: NetParams -> RVar NetData
randomNetwork net = do
    let nodes = [Node i | i <- [1..nNodes net]]
        items = [Item i | i <- [1..nItems net]]
        nodeItem = do node <- randomElement nodes
                      item <- randomElement items
                      return (node, item)
    edges <- replicateM (nItemsPerNode net) nodeItem
    return $ NetData { dAlphaTheta = 0.1
                     , dAlphaPhi   = 0.1
                     , dNodes      = S.fromList nodes
                     , dItems      = S.fromList items
                     , dTopics     = S.fromList [Topic i | i <- [1..nTopics net]]
                     , dNodeItems  = setupNodeItems edges
                     }

data LDABenchmark = LDABenchmark { bNetParams    :: NetParams
                                 , bThreads      :: Int
                                 , bUpdateBlock  :: Int
                                 , bSweeps       :: Int
                                 }

drawLdaBenchmark :: LDABenchmark -> RVar Benchmark
drawLdaBenchmark b = do
    net <- randomNetwork $ bNetParams b
    init <- randomInitialize net
    let name = printf "%d topics, %d threads, %d block, %d items per node" (nTopics $ bNetParams b) (bThreads b) (bUpdateBlock b) (nItemsPerNode $ bNetParams b)
    return $ bench name $ do
        gibbsUpdate (bThreads b) (bUpdateBlock b) (model net init)
            $ concat $ replicate (bSweeps b) (updateUnits net)

ldaBenchmarkParams :: [LDABenchmark]
ldaBenchmarkParams = do
    updateBlock <- [10, 100, 1000]
    topics <- [20, 100, 500, 1000]
    nItemsPerNode <- [200]
    threads <- [1, 2, 3, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26]
    return LDABenchmark { bNetParams = netParams {nTopics=topics, nItemsPerNode=nItemsPerNode}
                        , bThreads = threads
                        , bUpdateBlock = updateBlock
                        , bSweeps = 2
                        }

ldaBenchmarks :: RVar Benchmark
ldaBenchmarks = bgroup "LDA" `fmap` mapM drawLdaBenchmark ldaBenchmarkParams

