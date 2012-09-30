{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import           Options.Applicative    
import           Data.Monoid ((<>))                 
import           System.FilePath.Posix ((</>))

import           Data.Vector (Vector)    
import qualified Data.Vector.Generic as V    
import           Statistics.Sample (mean)       

import qualified Data.Bimap as BM                 
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M

import           ReadData       
import qualified RunSampler as Sampler
import           BayesStack.DirMulti
import           BayesStack.Models.Topic.SharedTaste

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
       
import           Data.Random
import           System.Random.MWC                 

import           Text.Printf
                 
data RunOpts = RunOpts { arcsFile        :: FilePath
                       , nodeItemsFile   :: FilePath
                       , stopwords       :: Maybe FilePath
                       , nTopics         :: Int
                       , samplerOpts     :: Sampler.SamplerOpts
                       }

runOpts = RunOpts 
    <$> strOption  ( long "arcs"
                   & metavar "FILE"
                   & value "arcs"
                   & help "File containing arcs"
                   )
    <*> strOption  ( long "items"
                   & metavar "FILE"
                   & value "node-items"
                   & help "File containing nodes' items"
                   )
    <*> nullOption ( long "stopwords"
                   & metavar "FILE"
                   & reader (Just . Just)
                   & value (Just "stopwords.txt")
                   & help "Stop words list"
                   )
    <*> option     ( long "topics"
                   & metavar "N"
                   & value 20
                   & help "Number of topics"
                   )
    <*> Sampler.samplerOpts

netData :: M.Map Node (Set Term) -> Set Edge -> Int -> NetData
netData nodeItems edges nTopics = 
    let items :: BM.Bimap Item Term
        items = BM.fromList $ zip [Item i | i <- [1..]] (S.toList $ S.unions $ M.elems nodeItems)
    in NetData { dAlphaPsi         = 0.1
               , dAlphaLambda      = 0.1
               , dAlphaPhi         = 0.1
               , dAlphaOmega       = 0.1
               , dAlphaGammaShared = 0.8
               , dAlphaGammaOwn    = 0.2
               , dEdges            = edges
               , dItems            = S.fromList $ BM.keys items
               , dTopics           = S.fromList [Topic i | i <- [1..nTopics]]
               , dNodeItems        = M.fromList
                                     $ zip [NodeItem i | i <- [0..]]
                                     $ do (n,terms) <- M.assocs nodeItems
                                          term <- S.toList terms
                                          return (n, items BM.!> term)
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
    nodeItems <- readNodeItems stopWords $ nodeItemsFile args
    let termCounts = V.fromListN (M.size nodeItems) $ map S.size $ M.elems nodeItems :: Vector Int
    printf "Read %d edges, %d items\n" (S.size edges) (M.size nodeItems)
    printf "Mean items per node:  %1.2f\n" (mean $ V.map realToFrac termCounts)
    
    withSystemRandom $ \mwc->do
    let nd = netData nodeItems edges 10
    mInit <- runRVar (randomInitialize nd) mwc
    let m = model nd mInit
    Sampler.runSampler (samplerOpts args) m (updateUnits nd)
    return ()
