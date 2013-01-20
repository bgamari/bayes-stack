import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State
import           Control.Monad (replicateM, forM)

import qualified Data.Set as S

import           Control.Concurrent (setNumCapabilities)
import           Criterion
import           Criterion.Config
import           Criterion.Main
import           Text.Printf

import           Data.Random
import           System.Random.MWC

import           BayesStack.Core.Gibbs
import           BayesStack.Models.Topic.LDA

data NetParams = NetParams { nNodes        :: Int
                           , nItems        :: Int
                           , nTopics       :: Int
                           , nItemsPerNode :: Int
                           }

netParams = NetParams { nNodes = 2000, nItems = 30000, nTopics = 100, nItemsPerNode = 20 }

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

withSystemRandomIO :: (GenIO -> IO a) -> IO a          
withSystemRandomIO = withSystemRandom                   

data LDABenchmark = LDABenchmark { bNetParams    :: NetParams
                                 , bThreads      :: Int
                                 , bUpdateBlock  :: Int
                                 , bSweeps       :: Int
                                 }
                                 
benchmarks = do
    updateBlock <- [10, 100, 1000]
    topics <- [20, 100, 500]
    nItemsPerNode <- [20, 200]
    threads <- [1,2,3, 4, 5, 6, 7, 8, 10]
    return LDABenchmark { bNetParams = netParams {nTopics=topics, nItemsPerNode=nItemsPerNode}
                        , bThreads = threads
                        , bUpdateBlock = updateBlock
                        , bSweeps = if nItemsPerNode == 20 then 40000 else 40000
                        }
                        
ldaBenchmark :: LDABenchmark -> RVar Benchmark
ldaBenchmark b = do
    net <- randomNetwork $ bNetParams b
    init <- randomInitialize net
    let name = printf "%d topics, %d threads, %d block, %d items per node" (nTopics $ bNetParams b) (bThreads b) (bUpdateBlock b) (nItemsPerNode $ bNetParams b)
    return $ bench name $ do
        setNumCapabilities $ bThreads b
        gibbsUpdate (bUpdateBlock b) (model net init) (take (bSweeps b) $ cycle $ updateUnits net)

main = do
    bs <- withSystemRandomIO $ runRVar (mapM ldaBenchmark benchmarks)
    defaultMainWith defaultConfig (return ())
        [ bgroup "LDA" bs ]

