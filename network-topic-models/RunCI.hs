{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import           Prelude hiding (mapM)

import           Options.Applicative
import           Data.Monoid ((<>))
import           Control.Monad.Trans.Class

import           Data.Vector (Vector)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as V
import           Statistics.Sample (mean)

import           Data.Traversable (mapM)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M

import           ReadData
import           SerializeText
import qualified RunSampler as Sampler
import           BayesStack.DirMulti
import           BayesStack.Models.Topic.CitationInfluence
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
                       , noClean         :: Bool
                       }

runOpts = RunOpts
    <$> strOption  ( long "arcs"
                  <> short 'a'
                  <> metavar "FILE"
                  <> help "File containing arcs"
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
    <*> flag False True ( long "no-clean"
                       <> short 'c'
                       <> help "Don't attempt to sanitize input data. Among other things, nodes without friends will not be discarded"
                        )

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

makeNetData :: HyperParams -> M.Map Node [Item] -> Set Arc -> Int -> NetData
makeNetData hp nodeItems arcs nTopics =
    netData hp arcs nodeItems' topics
  where topics = S.fromList [Topic i | i <- [1..nTopics]]
        nodeItems' = M.fromList
                     $ zip [NodeItem i | i <- [0..]]
                     $ do (n,items) <- M.assocs nodeItems
                          item <- items
                          return (n, item)

opts = info runOpts
           (  fullDesc
           <> progDesc "Learn citation influence model"
           <> header "run-ci - learn citation influence model"
           )

edgesToArcs :: Set (Node, Node) -> Set Arc
edgesToArcs = S.map (\(a,b)->Arc (Citing a) (Cited b))

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
    let arcs = edgesToArcs a

    Sampler.createSweeps $ samplerOpts args
    let sweepsDir = Sampler.sweepsDir $ samplerOpts args
    encodeFile (sweepsDir </> "item-map") itemMap
    encodeFile (sweepsDir </> "node-map") nodeMap

    let termCounts = V.fromListN (M.size nodeItems)
                     $ map length $ M.elems nodeItems :: Vector Int
    printf "Read %d arcs, %d nodes, %d node-items\n" (S.size arcs) (M.size nodeItems) (V.sum termCounts)
    printf "Mean items per node:  %1.2f\n" (mean $ V.map realToFrac termCounts)

    withSystemRandom $ \mwc->do
    let nd = (if noClean args then id else cleanNetData)
             $ makeNetData (hyperParams args) nodeItems arcs (nTopics args)
    mapM_ putStrLn $ verifyNetData (\n->maybe (show n) show $ M.lookup n nodeMap) nd

    let nCitingNodes = VU.fromList $ M.elems $ M.unionsWith (+)
                       $ map (\a->M.singleton (citingNode a) 1)
                       $ S.toList $ dArcs nd

        nCitedNodes  = VU.fromList $ M.elems $ M.unionsWith (+)
                       $ map (\a->M.singleton (citedNode a) 1)
                       $ S.toList $ dArcs nd
    printf "After cleaning: %d cited nodes, %d citing nodes, %d arcs, %d node-items\n"
           (S.size $ S.map citedNode $ dArcs nd) (S.size $ S.map citingNode $ dArcs nd)
           (S.size $ dArcs nd) (M.size $ dNodeItems nd)
    printf "In degree:  mean=%3.1f, maximum=%3.1f\n"
           (mean nCitedNodes) (V.maximum nCitedNodes)
    printf "Out degree: mean=%3.1f, maximum=%3.1f\n"
           (mean nCitingNodes) (V.maximum nCitingNodes)

    encodeFile (sweepsDir </> "data") nd
    mInit <- runRVar (randomInitialize nd) mwc
    let m = model nd mInit
    Sampler.runSampler (samplerOpts args) m (updateUnits nd)
    return ()
