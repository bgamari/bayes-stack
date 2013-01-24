module BenchST where

import           Control.Monad.Trans.State
import           Control.Monad (replicateM, forM, guard)
import           Control.Applicative ((<$>))
import           Text.Printf

import           Criterion
import           Data.Random

import qualified Data.Set as S
import           Data.List ((\\))
import           BayesStack.Core.Gibbs
import           BayesStack.Models.Topic.SharedTaste

data NetParams = NetParams { nNodes        :: Int
                           , nEdgesPerNode :: Int
                           , nItems        :: Int
                           , nTopics       :: Int
                           , nItemsPerNode :: Int
                           }

netParams = NetParams { nNodes = 5000
                      , nEdgesPerNode = 10
                      , nItems = nItemsPerNode netParams * nNodes netParams `div` 10
                      , nTopics = 100
                      , nItemsPerNode = 20
                      }

randomNetwork :: NetParams -> RVar NetData
randomNetwork net = do
    let nodes = [Node i | i <- [1..nNodes net]]
        items = [Item i | i <- [1..nItems net]]
        edge a = do b <- randomElement nodes --(nodes \\ [a])
                    return $ Edge (a,b) 
        nodeItem = do node <- randomElement nodes
                      item <- randomElement items
                      return (node, item)
    edges <- concat <$> forM nodes (replicateM (nEdgesPerNode net) . edge)
    nodeItems <- replicateM (nItemsPerNode net) nodeItem
    return $ NetData { dAlphaLambda= 0.1
                     , dAlphaPhi   = 0.1
                     , dAlphaPsi   = 0.01
                     , dAlphaOmega = 0.1
                     , dAlphaGammaShared = 0.9
                     , dAlphaGammaOwn = 0.1
                     , dEdges      = S.fromList edges
                     , dItems      = S.fromList items
                     , dTopics     = S.fromList [Topic i | i <- [1..nTopics net]]
                     , dNodeItems  = setupNodeItems nodeItems
                     }

data STBenchmark = STBenchmark { bNetParams    :: NetParams
                               , bThreads      :: Int
                               , bUpdateBlock  :: Int
                               , bSweeps       :: Int
                               }

drawStBenchmark :: STBenchmark -> RVar Benchmark
drawStBenchmark b = do
    net <- randomNetwork $ bNetParams b
    init <- randomInitialize net
    let name = printf "%d topics, %d threads, %d block, %d items per node" (nTopics $ bNetParams b) (bThreads b) (bUpdateBlock b) (nItemsPerNode $ bNetParams b)
    return $ bench name $ do
        gibbsUpdate (bThreads b) (bUpdateBlock b) (model net init)
            $ concat $ replicate (bSweeps b) (updateUnits net)

stBenchmarkParams :: [STBenchmark]
stBenchmarkParams = do
    updateBlock <- [10, 100, 1000]
    topics <- [20, 100, 500]
    threads <- [1, 2, 3, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26]
    return STBenchmark { bNetParams = netParams {nTopics=topics}
                       , bThreads = threads
                       , bUpdateBlock = updateBlock
                       , bSweeps = 2
                       }

stBenchmarks :: RVar Benchmark
stBenchmarks = bgroup "ST" `fmap` mapM drawStBenchmark stBenchmarkParams

