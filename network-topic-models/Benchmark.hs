import           Criterion
import           Criterion.Config
import           Criterion.Main
import           Data.Random
import           System.Random.MWC
import           BenchLDA
import           BenchST

benchmarks :: RVar [Benchmark]
benchmarks = sequence [ldaBenchmarks, stBenchmarks]

<<<<<<< HEAD
withSystemRandomIO :: (GenIO -> IO a) -> IO a
withSystemRandomIO = withSystemRandom
=======
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
        --setNumCapabilities $ bThreads b
        gibbsUpdate (bThreads b) (bUpdateBlock b) (model net init) (take (bSweeps b) $ cycle $ updateUnits net)
>>>>>>> bgamari/master

main = do
    bs <- withSystemRandomIO $ runRVar benchmarks
    defaultMainWith defaultConfig (return ()) bs

