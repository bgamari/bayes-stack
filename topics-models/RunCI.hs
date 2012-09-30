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
import           Data.Maybe (mapMaybe)       

import           Control.Applicative
import           Control.Monad (when, forM_)                
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as S

import           BayesStack.Core
import           BayesStack.Models.Topic.CitationInfluence

import           Data.Char (isAlpha)                 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Read (decimal)
       
import           Data.Random
import           System.Random.MWC                 

import           Data.Number.LogFloat (LogFloat, logFromLogFloat)

import           Text.Printf
import qualified Data.ByteString as BS
import Data.Serialize

import           Control.Concurrent
import           Control.Concurrent.STM       
                 
data RunCIOpts = RunCIOpts { arcsFile        :: FilePath
                           , nodeItemsFile   :: FilePath
                           , stopwords       :: Maybe FilePath
                           , sweepsDir       :: FilePath
                           , sweepBlockSize  :: Int
                           , iterations      :: Maybe Int
                           , nTopics         :: Int
                           }

runCIOpts = RunCIOpts 
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
    <*> strOption  ( long "sweeps"
                   & metavar "DIR"
                   & value "sweeps"
                   & help "Directory in which to place sweeps"
                   )
    <*> option     ( long "sweep-block"
                   & metavar "N"
                   & value 10
                   & help "Number of sweeps to dispatch at once"
                   )
    <*> option     ( long "iterations"
                   & metavar "N"
                   & value Nothing
                   & reader (Just . auto)
                   & help "Number of sweep blocks to run for"
                   )
    <*> option     ( long "topics"
                   & metavar "N"
                   & value 20
                   & help "Number of topics"
                   )

readArcs :: FilePath -> IO (Set Arc)
readArcs fname =
    S.fromList . mapMaybe parseLine . T.lines <$> TIO.readFile fname
    where parseLine :: T.Text -> Maybe Arc
          parseLine l = case T.words l of
             [a,b] -> case (decimal a, decimal b) of
                          (Right (a',_), Right (b',_)) ->
                              Just $ Arc (Citing (Node a'), Cited (Node b'))
                          otherwise -> Nothing
             otherwise -> Nothing

type Term = T.Text
readNodeItems :: Set Term -> FilePath -> IO (M.Map Node (Set Term))
readNodeItems stopWords fname =
    M.unionsWith S.union . map parseLine . T.lines <$> TIO.readFile fname
    where parseLine :: T.Text -> M.Map Node (Set Term)
          parseLine l = case T.words l of
             n:words | Right (n',_) <- decimal n ->
                 M.singleton (Node n')
                 $ S.fromList
                 $ filter (\word->T.length word > 4)
                 $ map (T.filter isAlpha)
                 $ filter (`S.notMember` stopWords) words
             otherwise -> M.empty

netData :: M.Map Node (Set Term) -> Set Arc -> Int -> NetData
netData abstracts arcs nTopics = cleanNetData $ 
    let items :: BM.Bimap Item Term
        items = BM.fromList $ zip [Item i | i <- [1..]] (S.toList $ S.unions $ M.elems abstracts)
    in NetData { dAlphaPsi         = 0.1
               , dAlphaLambda      = 0.1
               , dAlphaPhi         = 0.1
               , dAlphaOmega       = 0.1
               , dAlphaGammaShared = 0.8
               , dAlphaGammaOwn    = 0.2
               , dArcs             = arcs
               , dItems            = S.fromList $ BM.keys items
               , dTopics           = S.fromList [Topic i | i <- [1..nTopics]]
               , dNodeItems        = M.fromList
                                     $ zip [NodeItem i | i <- [0..]]
                                     $ do (n,terms) <- M.assocs abstracts
                                          term <- S.toList terms
                                          return (n, items BM.!> term)
               }
            
opts = info (runCIOpts)
           (  fullDesc
           <> progDesc "Learn citation influence model"
           <> header "run-ci - learn citation influence model"
           )

serializeState :: MState -> FilePath -> IO ()
serializeState model fname = liftIO $ BS.writeFile fname $ runPut $ put model

processSweep :: FilePath -> TVar LogFloat -> Int -> MState -> IO ()
processSweep sweepsDir lastMaxV sweepN m = do             
    let l = modelLikelihood m
    putStr $ printf "Sweep %d: %f\n" sweepN (logFromLogFloat l :: Double)
    newMax <- atomically $ do oldL <- readTVar lastMaxV
                              if l > oldL then writeTVar lastMaxV l >> return True
                                          else return False
    when newMax
        $ serializeState m $ printf "%s/%05d" sweepsDir sweepN

main = do
    args <- execParser $ opts
    stopWords <- case stopwords args of
                     Just f  -> S.fromList . T.words <$> TIO.readFile f
                     Nothing -> return S.empty
    printf "Read %d stopwords\n" (S.size stopWords)

    arcs <- readArcs $ arcsFile args
    abstracts <- readNodeItems stopWords $ nodeItemsFile args
    let termCounts = V.fromListN (M.size abstracts) $ map S.size $ M.elems abstracts :: Vector Int
    printf "Read %d arcs, %d abstracts\n" (S.size arcs) (M.size abstracts)
    printf "Mean terms per document:  %1.2f\n" (mean $ V.map realToFrac termCounts)
    
    withSystemRandom $ \mwc->do
    let nd = netData abstracts arcs 10
    print $ verifyNetData nd
    init <- runRVar (randomInitialize nd) mwc
    let m = model nd init
        uus = updateUnits nd
    
    putStrLn "Starting inference"
    lastMaxV <- atomically $ newTVar 0
    let update :: Int -> S.StateT MState IO ()
        update blockN = do
            m <- S.get
            let sweepN = blockN * sweepBlockSize args
            liftIO $ forkIO $ processSweep (sweepsDir args) lastMaxV sweepN m
            m' <- liftIO $ gibbsUpdate m $ concat $ replicate (sweepBlockSize args) uus
            S.put m'
    
    let nBlocks = maybe [0..] (\n->[0..n]) $ iterations args
    S.runStateT (forM_ nBlocks update :: S.StateT MState IO ()) m
    return ()
