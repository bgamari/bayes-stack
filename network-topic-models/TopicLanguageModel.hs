{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Foldable as F
import           Data.List (sort, sortBy, isSuffixOf)
import           Data.Function
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Tuple (swap)
import qualified Data.Traversable as T
import           Options.Applicative

import qualified Data.Text.Lazy.IO as TL
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Lazy.Builder as TB
import           Data.Binary

import           System.Directory
import           System.FilePath ((</>), takeDirectory)
import           Text.Printf

import           BayesStack.Types
import           BayesStack.DirMulti
import           BayesStack.Models.Topic.LDARelevance
import           SerializeText
import           ReadData
import           FormatMultinom

data Opts = Opts { inputFiles   :: [FilePath]
                 , sweepNum     :: Int
                 , documents    :: [NodeName]
                 , topN         :: Maybe Int
                 }

opts = Opts
    <$> arguments pure
                  ( metavar "DIR|FILE"
                 <> help "The directory of sweeps to dump"
                  )
    <*> option       ( long "number"
                    <> short 'N'
                    <> value 10
                    <> metavar "N"
                    <> help "The number of sweeps to aggregate"
                     )
    <*> option       ( long "document"
                    <> short 'd'
                    <> reader (fmap ((:[]) . Text.pack) . str)
                    <> help "The documents to dump"
                     )
    <*> option       ( long "top"
                    <> short 'n'
                    <> value Nothing
                    <> reader (pure . auto)
                    <> metavar "N"
                    <> help "How many items to list for each"
                     )

readSweep :: FilePath -> IO MState
readSweep = decodeFile

readNetData :: FilePath -> IO NetData
readNetData = decodeFile

readPhi' :: Node -> FilePath -> IO (Map Item Double)
readPhi' n filePath = f <$> readSweep filePath
  where f s = M.unionsWith (+) $ do
            (pt,t) <- F.toList $ probabilities $ stThetas s M.! n
            (px,x) <- F.toList $ probabilities $ stPhis s M.! t
            return $ M.singleton x (pt*px)

readPhi :: [NodeName] -> FilePath -> IO (Map NodeName (Map Term Double))
readPhi nodes filePath = do
    let sweepDir = takeDirectory filePath
    itemMap <- readItemMap sweepDir
    nodeMap <- readNodeMap sweepDir
    let invNodeMap = M.fromList $ map swap $ M.assocs nodeMap
    let f n = let n' = invNodeMap M.! n
              in M.singleton n . M.mapKeys (itemMap M.!) <$> readPhi' n' filePath
    M.unionsWith (M.unionWith (+)) <$> mapM f nodes

getSweeps :: FilePath -> IO [FilePath]
getSweeps sweepsDir =
    map (sweepsDir </>) . sort . filter (".state" `isSuffixOf`)
    <$> getDirectoryContents sweepsDir

main = do
    args <- execParser $ info (helper <*> opts)
         ( fullDesc
        <> progDesc "Dump distributions from an LDA sweep"
        <> header "dump-lda - Dump distributions from an LDA sweep"
         )

    --nd <- readNetData $ sweepDir args </> "data"

    files <- T.forM (inputFiles args) $ \fname->do
        isDir <- doesDirectoryExist fname
        isFile <- doesFileExist fname
        case () of
            _ | isFile    ->
                return [fname]
            _ | isDir     ->
                take (sweepNum args) . reverse <$> getSweeps fname
            _ | otherwise -> error "oops"

    sweeps <- T.forM (concat files) $ readPhi (documents args)
    let sweeps' = M.unionsWith (M.unionWith (+)) sweeps
                :: Map NodeName (Map Term Double)
        takeTopN = maybe id take $ topN args
    F.forM_ (M.assocs sweeps') $ \(n,items)->do
        print n
        F.forM_ (takeTopN $ sortBy (flip compare `on` snd) $ M.assocs items) $ \(x,p)->do
            printf "\t%s\t%1.2e\n" (show x) p
        putStrLn ""
