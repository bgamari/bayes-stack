{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import           Prelude hiding (mapM)

import           Options.Applicative
import           Data.Monoid ((<>))
import           Control.Monad.Trans.Class

import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import           Statistics.Sample (mean)

import           Data.Traversable (mapM)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M

import           ReadData
import           SerializeText
import qualified RunSampler as Sampler
import           BayesStack.Multinomial
import           BayesStack.Models.Topic.SharedTaste
import           BayesStack.UniqueKey

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           System.FilePath.Posix ((</>))
import           Data.Binary
import qualified Data.ByteString as BS
import           Text.Printf

import           Data.Random
import           System.Random.MWC

data RunOpts = RunOpts { arcsFile        :: FilePath
                       , nodesFile       :: FilePath
                       , stopwords       :: Maybe FilePath
                       , nTopics         :: Int
                       , samplerOpts     :: Sampler.SamplerOpts
                       , hyperParams     :: HyperParams
                       }

runOpts = RunOpts
    <$> strOption  ( long "edges"
                  <> short 'e'
                  <> metavar "FILE"
                  <> help "File containing edges"
                   )
    <*> strOption  ( long "nodes"
                  <> short 'n'
                  <> metavar "FILE"
                  <> help "File containing nodes' items"
                   )
    <*> nullOption ( long "stopwords"
                  <> short 's'
                  <> metavar "FILE"
                  <> reader (pure . Just)
                  <> value Nothing
                  <> help "Stop words list"
                   )
    <*> option     ( long "topics"
                  <> short 't'
                  <> metavar "N"
                  <> value 20
                  <> help "Number of topics"
                   )
    <*> Sampler.samplerOpts
    <*> hyperOpts

hyperOpts = HyperParams
    <$> option     ( long "prior-psi"
                  <> value 1
                  <> help "Dirichlet parameter for prior on psi"
                   )
    <*> option     ( long "prior-lambda"
                  <> value 0.1
                  <> help "Dirichlet parameter for prior on lambda"
                   )
    <*> option     ( long "prior-phi"
                  <> value 0.01
                  <> help "Dirichlet parameter for prior on phi"
                   )
    <*> option     ( long "prior-omega"
                  <> value 0.01
                  <> help "Dirichlet parameter for prior on omega"
                   )
    <*> option     ( long "prior-gamma-shared"
                  <> value 0.9
                  <> help "Beta parameter for prior on gamma (shared)"
                   )
    <*> option     ( long "prior-gamma-own"
                  <> value 0.1
                  <> help "Beta parameter for prior on gamma (own)"
                   )

mapMKeys :: (Ord k, Ord k', Monad m, Applicative m)
         => (a -> m a') -> (k -> m k') -> M.Map k a -> m (M.Map k' a')
mapMKeys f g x = M.fromList <$> (mapM (\(k,v)->(,) <$> g k <*> f v) $ M.assocs x)

termsToItems :: M.Map NodeName [Term] -> Set (NodeName, NodeName)
             -> ( (M.Map Node [Item], Set (Node, Node))
                , (M.Map Item Term, M.Map Node NodeName))
termsToItems nodes arcs =
    let ((d', nodeMap), itemMap) =
            runUniqueKey' [Item i | i <- [0..]] $
            runUniqueKeyT' [Node i | i <- [0..]] $ do
                a <- mapMKeys (mapM (lift . getUniqueKey)) getUniqueKey nodes
                b <- S.fromList <$> mapM (\(x,y)->(,) <$> getUniqueKey x <*> getUniqueKey y)
                     (S.toList arcs)
                return (a,b)
    in (d', (itemMap, nodeMap))

netData :: HyperParams -> M.Map Node [Item] -> Set Edge -> Int -> NetData
netData hp nodeItems edges nTopics =
    NetData { dHypers           = hp
            , dEdges            = edges
            , dItems            = S.unions $ map S.fromList $ M.elems nodeItems
            , dTopics           = S.fromList [Topic i | i <- [1..nTopics]]
            , dNodeItems        = M.fromList
                                  $ zip [NodeItem i | i <- [0..]]
                                  $ do (n,items) <- M.assocs nodeItems
                                       item <- items
                                       return (n, item)
            }

opts = info runOpts
           (  fullDesc
           <> progDesc "Learn shared taste model"
           <> header "run-st - learn shared taste model"
           )

instance Sampler.SamplerModel MState where
    estimateHypers = id -- reestimate -- FIXME
    modelLikelihood = modelLikelihood
    summarizeHypers ms =  "" -- FIXME

main = do
    args <- execParser opts
    stopWords <- case stopwords args of
                     Just f  -> S.fromList . T.words <$> TIO.readFile f
                     Nothing -> return S.empty
    printf "Read %d stopwords\n" (S.size stopWords)

    ((nodeItems, a), (itemMap, nodeMap)) <- termsToItems
                            <$> readNodeItems stopWords (nodesFile args)
                            <*> readEdges (arcsFile args)
    let edges = S.map Edge a

    Sampler.createSweeps $ samplerOpts args
    let sweepsDir = Sampler.sweepsDir $ samplerOpts args
    encodeFile (sweepsDir </> "item-map") itemMap
    encodeFile (sweepsDir </> "node-map") nodeMap

    let termCounts = V.fromListN (M.size nodeItems)
                     $ map length $ M.elems nodeItems :: Vector Int
    printf "Read %d edges, %d items\n" (S.size edges) (M.size nodeItems)
    printf "Mean items per node:  %1.2f\n" (mean $ V.map realToFrac termCounts)

    withSystemRandom $ \mwc->do
    let nd = netData (hyperParams args) nodeItems edges 10
    encodeFile (sweepsDir </> "data") nd
    mInit <- runRVar (randomInitialize nd) mwc
    let m = model nd mInit
    Sampler.runSampler (samplerOpts args) m (updateUnits nd)
    return ()
