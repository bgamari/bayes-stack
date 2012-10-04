{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import           Prelude hiding (mapM)    

import           Options.Applicative    
import           Data.Monoid ((<>))                 

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
import           BayesStack.DirMulti
import           BayesStack.Models.Topic.SharedTaste
import           BayesStack.UniqueKey

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath.Posix ((</>))
import           Data.Serialize
import qualified Data.ByteString as BS
import           Text.Printf
       
import           Data.Random
import           System.Random.MWC                 
                 
data RunOpts = RunOpts { arcsFile        :: FilePath
                       , nodesFile       :: FilePath
                       , stopwords       :: Maybe FilePath
                       , nTopics         :: Int
                       , samplerOpts     :: Sampler.SamplerOpts
                       }

runOpts = RunOpts 
    <$> strOption  ( long "edges"
                  <> short 'e'
                  <> metavar "FILE"
                  <> value "arcs"
                  <> help "File containing arcs"
                   )
    <*> strOption  ( long "nodes"
                  <> short 'n'
                  <> metavar "FILE"
                  <> value "node-items"
                  <> help "File containing nodes' items"
                   )
    <*> nullOption ( long "stopwords"
                  <> short 's'
                  <> metavar "FILE"
                  <> reader (Just . Just)
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
    
termsToItems :: M.Map Node [Term] -> (M.Map Node [Item], M.Map Item Term)
termsToItems = runUniqueKey' [Item i | i <- [0..]]
            . mapM (mapM getUniqueKey)

netData :: M.Map Node [Item] -> Set Edge -> Int -> NetData
netData nodeItems edges nTopics = 
    NetData { dAlphaPsi         = 0.1
            , dAlphaLambda      = 0.1
            , dAlphaPhi         = 0.1
            , dAlphaOmega       = 0.1
            , dAlphaGammaShared = 0.8
            , dAlphaGammaOwn    = 0.2
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
           <> progDesc "Learn citation influence model"
           <> header "run-ci - learn citation influence model"
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

    edges <- S.map Edge <$> readEdges (arcsFile args)
    (nodeItems, itemMap) <- termsToItems
                            <$> readNodeItems stopWords (nodesFile args)

    let sweepsDir = Sampler.sweepsDir $ samplerOpts args
    createDirectoryIfMissing False sweepsDir
    BS.writeFile (sweepsDir </> "item-map") $ runPut $ put itemMap

    let termCounts = V.fromListN (M.size nodeItems)
                     $ map length $ M.elems nodeItems :: Vector Int
    printf "Read %d edges, %d items\n" (S.size edges) (M.size nodeItems)
    printf "Mean items per node:  %1.2f\n" (mean $ V.map realToFrac termCounts)
    
    withSystemRandom $ \mwc->do
    let nd = netData nodeItems edges 10
    mInit <- runRVar (randomInitialize nd) mwc
    let m = model nd mInit
    Sampler.runSampler (samplerOpts args) m (updateUnits nd)
    return ()

