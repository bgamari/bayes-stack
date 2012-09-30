{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import           Options.Applicative    
import           Data.Monoid ((<>))                 

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
import           BayesStack.Models.Topic.LDA

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
       
import           Data.Random
import           System.Random.MWC                 

import           Text.Printf
                 
data RunOpts = RunOpts { nodesFile       :: FilePath
                       , stopwords       :: Maybe FilePath
                       , nTopics         :: Int
                       , samplerOpts     :: Sampler.SamplerOpts
                       }

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
                  <> reader (Just . Just)
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


netData :: M.Map Node (Set Term) -> Int -> NetData
netData nodeItems nTopics = 
    let items :: BM.Bimap Item Term
        items = BM.fromList $ zip [Item i | i <- [1..]] (S.toList $ S.unions $ M.elems nodeItems)
    in NetData { dAlphaTheta       = 0.1
               , dAlphaPhi         = 0.1
               , dItems            = S.fromList $ BM.keys items
               , dTopics           = S.fromList [Topic i | i <- [1..nTopics]]
               , dNodeItems        = M.fromList
                                     $ zip [NodeItem i | i <- [0..]]
                                     $ do (n,terms) <- M.assocs nodeItems
                                          term <- S.toList terms
                                          return (n, items BM.!> term)
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

    nodeItems <- readNodeItems stopWords $ nodesFile args
    let termCounts = V.fromListN (M.size nodeItems) $ map S.size $ M.elems nodeItems :: Vector Int
    printf "Read %d nodes\n" (M.size nodeItems)
    printf "Mean items per node:  %1.2f\n" (mean $ V.map realToFrac termCounts)
    
    withSystemRandom $ \mwc->do
    let nd = netData nodeItems (nTopics args)
    mInit <- runRVar (randomInitialize nd) mwc
    let m = model nd mInit
    Sampler.runSampler (samplerOpts args) m (updateUnits nd)
    return ()
