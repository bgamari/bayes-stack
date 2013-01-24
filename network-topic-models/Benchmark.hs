import           Criterion
import           Criterion.Config
import           Criterion.Main
import           Data.Random
import           System.Random.MWC
import           BenchLDA
import           BenchST

benchmarks :: RVar [Benchmark]
benchmarks = sequence [ldaBenchmarks] --, stBenchmarks]

withSystemRandomIO :: (GenIO -> IO a) -> IO a
withSystemRandomIO = withSystemRandom

main = do
    bs <- withSystemRandomIO $ runRVar benchmarks
    defaultMainWith defaultConfig (return ()) bs

