{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, StandaloneDeriving, TupleSections #-}

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
import qualified Data.Map.Strict as M

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           ReadData hiding (readNodeItems)
import           ReadRelevanceData (readNodeItems)
import           SerializeText
import qualified RunSampler as Sampler
import           BayesStack.DirMulti
import           BayesStack.Models.Topic.LDARelevance
import           BayesStack.UniqueKey

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath.Posix ((</>))
import           Data.Binary
import qualified Data.ByteString as BS
import           Text.Printf

import           Data.Random
import           System.Random.MWC

data RunOpts = RunOpts { nodesFile       :: FilePath
                       , stopwords       :: Maybe FilePath
                       , nTopics         :: Int
                       , samplerOpts     :: Sampler.SamplerOpts
                       , hyperParams     :: HyperParams
                       }

data HyperParams = HyperParams
                   { alphaTheta       :: Double
                   , alphaPhi         :: Double
                   }
                 deriving (Show, Eq)

runOpts :: Parser RunOpts
runOpts = RunOpts
    <$> strOption  ( long "nodes"
                  <> short 'n'
                  <> metavar "FILE"
                  <> help "File containing nodes and their associated items"
                   )
    <*> nullOption ( long "stopwords"
                  <> short 's'
                  <> metavar "FILE"
                  <> reader (pure . Just)
                  <> value Nothing
                  <> help "Stop word list"
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
    <$> option     ( long "prior-theta"
                  <> value 1
                  <> help "Dirichlet parameter for prior on theta"
                   )
    <*> option     ( long "prior-phi"
                  <> value 0.1
                  <> help "Dirichlet parameter for prior on phi"
                   )

mapMKeys :: (Ord k, Ord k', Monad m, Applicative m)
         => (a -> m a') -> (k -> m k') -> M.Map k a -> m (M.Map k' a')
mapMKeys f g x = M.fromList <$> (mapM (\(k,v)->(,) <$> g k <*> f v) $ M.assocs x)

termsToItems :: M.Map NodeName [(Term, ItemWeight)]
             -> (M.Map Node [(Item, ItemWeight)], (M.Map Item Term, M.Map Node NodeName))
termsToItems nodes =
    let ((d', nodeMap), itemMap) =
            runUniqueKey' [Item i | i <- [0..]] $
            runUniqueKeyT' [Node i | i <- [0..]] $ do
                mapMKeys (mapM (\(x,w)->(,w) `fmap` lift (getUniqueKey x))) getUniqueKey nodes
    in (d', (itemMap, nodeMap))

netData :: HyperParams -> M.Map Node [(Item,ItemWeight)] -> Int -> NetData
netData hp nodeItems nTopics =
    NetData { dAlphaTheta       = alphaTheta hp
            , dAlphaPhi         = alphaPhi hp
            , dItems            = M.unions $ map M.fromList $ M.elems nodeItems
            , dTopics           = S.fromList [Topic i | i <- [1..nTopics]]
            , dNodeItems        = M.fromList
                                  $ zip [NodeItem i | i <- [0..]]
                                  $ do (n,items) <- M.assocs nodeItems
                                       (item,weight) <- items
                                       return (n, item)
            , dNodes            = M.keysSet nodeItems
            }

opts :: ParserInfo RunOpts
opts = info runOpts (  fullDesc
                    <> progDesc "Learn LDA model"
                    <> header "run-lda - learn LDA model"
                    )

instance Sampler.SamplerModel MState where
    estimateHypers = reestimate
    modelLikelihood = modelLikelihood
    summarizeHypers ms =
        "  phi  : "++show (dmAlpha $ snd $ M.findMin $ stPhis ms)++"\n"++
        "  theta: "++show (dmAlpha $ snd $ M.findMin $ stThetas ms)++"\n"

main :: IO ()
main = do
    args <- execParser opts
    stopWords <- case stopwords args of
                     Just f  -> S.fromList . T.words <$> TIO.readFile f
                     Nothing -> return S.empty
    printf "Read %d stopwords\n" (S.size stopWords)

    let parseError s = do
            putStrLn $ "Error parsing nodes: "++s
            putStrLn $ "Expected format is:"
            putStrLn $ "(node)  (term) (weight)  (term) (weight) ..."
            fail ""
    (nodeItems, (itemMap, nodeMap)) <- either parseError (return . termsToItems)
                            =<< readNodeItems stopWords (nodesFile args)

    let sweepsDir = Sampler.sweepsDir $ samplerOpts args
    Sampler.createSweeps $ samplerOpts args
    encodeFile (sweepsDir </> "item-map") itemMap
    encodeFile (sweepsDir </> "node-map") nodeMap

    let termCounts = V.fromListN (M.size nodeItems)
                     $ map length $ M.elems nodeItems :: Vector Int
    printf "Read %d nodes\n" (M.size nodeItems)
    printf "Mean items per node:  %1.2f\n" (mean $ V.map realToFrac termCounts)

    withSystemRandom $ \mwc->do
    let nd = netData (hyperParams args) nodeItems (nTopics args)
    encodeFile (sweepsDir </> "data") nd
    mInit <- runRVar (randomInitialize nd) mwc
    let m = model nd mInit
    Sampler.runSampler (samplerOpts args) m (updateUnits nd)
    return ()
