{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Options.Applicative

import qualified Data.Map as M
import qualified Data.ByteString as BS

import qualified Data.Text.Lazy.IO as TL
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Lazy.Builder as TB
import           Data.Binary

import           System.FilePath ((</>))
import           Text.Printf

import           BayesStack.Models.Topic.LDA
import           SerializeText
import           ReadData
import           FormatMultinom

data Opts = Opts { nElems   :: Maybe Int
                 , dumper   :: Dumper
                 , sweepDir :: FilePath
                 , sweepNum :: Maybe Int
                 }

type Dumper = Opts -> NetData -> MState
              -> (Item -> TB.Builder) -> (Node -> TB.Builder)
              -> TB.Builder

showB :: Show a => a -> TB.Builder
showB = TB.fromString . show

readDumper :: String -> Maybe Dumper
readDumper "thetas" = Just $ \opts nd m showItem showNode ->
    formatMultinoms showNode showB (nElems opts) (stThetas m)

readDumper "phis"   = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Topic n)->"Topic "<>decimal n) showItem (nElems opts) (stPhis m)

readDumper _        = Nothing

opts = Opts
    <$> nullOption   ( long "top"
                    <> short 'n'
                    <> value Nothing
                    <> reader (pure . auto)
                    <> metavar "N"
                    <> help "Number of elements to output from each distribution"
                     )
    <*> argument readDumper
                     ( metavar "STR"
                    <> help "One of: thetas, lambdas"
                     )
    <*> strOption    ( long "sweeps"
                    <> short 's'
                    <> value "sweeps"
                    <> metavar "DIR"
                    <> help "The directory of sweeps to dump"
                     )
    <*> option       ( long "number"
                    <> short 'N'
                    <> reader (pure . auto)
                    <> value Nothing
                    <> metavar "N"
                    <> help "The sweep number to dump"
                     )

readSweep :: FilePath -> IO MState
readSweep = decodeFile

readNetData :: FilePath -> IO NetData
readNetData = decodeFile

main = do
    args <- execParser $ info (helper <*> opts)
         ( fullDesc
        <> progDesc "Dump distributions from an LDA sweep"
        <> header "dump-lda - Dump distributions from an LDA sweep"
         )

    nd <- readNetData $ sweepDir args </> "data"
    itemMap <- readItemMap $ sweepDir args
    nodeMap <- readNodeMap $ sweepDir args
    m <- case sweepNum args of
             Nothing -> readSweep =<< getLastSweep (sweepDir args)
             Just n  -> readSweep $ sweepDir args </> printf "%05d.state" n

    let showItem = showB . (itemMap M.!)
        showNode = showB . (nodeMap M.!)
    TL.putStr $ TB.toLazyText $ dumper args args nd m showItem showNode
