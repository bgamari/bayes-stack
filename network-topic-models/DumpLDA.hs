{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Options.Applicative

import qualified Data.Map as M
import qualified Data.ByteString as BS

import qualified Data.Text.Lazy.IO as TL
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Lazy.Builder as TB
import           Data.Serialize

import           BayesStack.Models.Topic.LDA
import           SerializeText
import           ReadData
import           FormatMultinom                 

data Opts = Opts { nElems  :: Int
                 , dist    :: Distribution
                 , sweep   :: FilePath
                 }
     
data Distribution = Phis | Thetas
     
readDistribution :: String -> Maybe Distribution
readDistribution "phis"   = Just Phis
readDistribution "thetas" = Just Thetas
readDistribution _        = Nothing

opts = Opts
    <$> option       ( long "n-elems"
                    <> short 'n'
                    <> value 30
                    <> help "Number of elements to output from each distribution"
                     )
    <*> nullOption   ( long "dist"
                    <> short 'd'
                    <> reader readDistribution
                    <> help "Which distribution to output (phis or thetas)"
                     )
    <*> argument str ( metavar "FILE" )

readItemMap :: IO (M.Map Item Term)                 
readItemMap =
    (either error id . runGet get) <$> BS.readFile "sweeps/item-map"

readSweep :: FilePath -> IO MState
readSweep fname = (either error id . runGet get) <$> BS.readFile fname

readNetData :: FilePath -> IO NetData
readNetData fname = (either error id . runGet get) <$> BS.readFile fname

dumpPhis :: Int -> M.Map Item Term -> MState -> TB.Builder
dumpPhis n itemMap m =
    formatMultinoms (\(Topic n)->"Topic "<>decimal n)
                    (TB.fromString . show . (itemMap M.!))
                    n (stPhis m)

dumpThetas :: Int -> MState -> TB.Builder
dumpThetas n m =
    formatMultinoms (\(Node n)->"Node "<>decimal n)
                    (TB.fromString . show)
                    n (stThetas m)

main = do
    args <- execParser $ info (helper <*> opts) 
         ( fullDesc 
        <> progDesc "Dump distributions from an LDA sweep"
        <> header "dump-lda - Dump distributions from an LDA sweep"
         )

    itemMap <- readItemMap
    m <- readSweep (sweep args)
    TL.putStr $ TB.toLazyText $ case dist args of
        Phis   -> dumpPhis (nElems args) itemMap m
        Thetas -> dumpThetas (nElems args) m

