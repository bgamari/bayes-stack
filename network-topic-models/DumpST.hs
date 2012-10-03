{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Options.Applicative

import qualified Data.Map as M
import qualified Data.ByteString as BS

import qualified Data.Text.Lazy.IO as TL
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Data.Serialize

import           BayesStack.Models.Topic.SharedTaste
import           ReadData
import           FormatMultinom                 
                 
data Opts = Opts { nElems  :: Int
                 , dist    :: Distribution
                 , sweep   :: FilePath
                 }
     
data Distribution = Phis | Psis | Lambdas
     
readDistribution :: String -> Maybe Distribution
readDistribution "phis"   = Just Phis
readDistribution "psis"   = Just Psis
readDistribution "lambdas"= Just Lambdas
readDistribution _        = Nothing

opts = Opts
    <$> option       ( long "n-elems"
                    <> short 'n'
                    <> value 30
                    <> help "Number of elements to output from each distributino"
                     )
    <*> nullOption   ( long "dist"
                    <> short 'd'
                    <> value Phis
                    <> reader readDistribution
                    <> help "Which distribution to output (phis or thetas)"
                     )
    <*> argument str ( metavar "FILE" )

readItemMap :: IO (M.Map Item Term)                 
readItemMap =
    (either error id . runGet get) <$> BS.readFile "sweeps/node-map"

readSweep :: FilePath -> IO MState
readSweep fname = (either error id . runGet get) <$> BS.readFile fname

readNetData :: FilePath -> IO NetData
readNetData fname = (either error id . runGet get) <$> BS.readFile fname

dumpPhis :: Int -> M.Map Item Term -> MState -> TB.Builder
dumpPhis n itemMap m =
    formatMultinoms (\(Topic n)->"Topic "<>decimal n)
                    (TB.fromString . show . (itemMap M.!))
                    n (stPhis m)

dumpPsis :: Int -> MState -> TB.Builder
dumpPsis n m =
    formatMultinoms (\(Node n)->"Node "<>decimal n)
                    (TB.fromString . show)
                    n (stPsis m)

dumpLambdas :: Int -> MState -> TB.Builder
dumpLambdas n m =
    formatMultinoms (TB.fromString . show)
                    (TB.fromString . show)
                    n (stLambdas m)

main = do
    args <- execParser $ info (helper <*> opts) 
         ( fullDesc 
        <> progDesc "Dump distributions from an shared taste model sweep"
        <> header "dump-lda - Dump distributions from an shared taste model sweep"
         )

    itemMap <- readItemMap
    m <- readSweep (sweep args)
    TL.putStr $ TB.toLazyText $ case dist args of
        Phis    -> dumpPhis (nElems args) itemMap m
        Psis    -> dumpPsis (nElems args) m
        Lambdas -> dumpLambdas (nElems args) m

instance Serialize T.Text where
     put = put . TE.encodeUtf8
     get = TE.decodeUtf8 <$> get
