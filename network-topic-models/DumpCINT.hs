{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens hiding (argument)
import           Data.Foldable
import           Data.Function (on)
import           Data.List
import           Data.Monoid
import           Options.Applicative

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as BS

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat
import           Data.Binary

import           System.FilePath ((</>))
import           Text.Printf

import           BayesStack.Models.Topic.CitationInfluenceNoTopics
import           FormatMultinom
import           Numeric.Log
import           ReadData
import           SerializeText

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

showTopic :: Topic -> TB.Builder
showTopic (Topic n) = "Topic "<>decimal n

formatProb = formatRealFloat Exponent (Just 3) . realToFrac

readDumper :: String -> Maybe Dumper
readDumper "psis"   = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Citing n)->showNode n) (\(Cited n)->showNode n) (nElems opts) (m^.stPsis)

readDumper "lambdas"= Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Cited n)->showNode n) showItem (nElems opts) (m^.stLambdas)

readDumper "omegas" = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Citing n)->showNode n) showB (nElems opts) (m^.stOmegas)

readDumper "gammas" = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Citing n)->showNode n) showB (nElems opts) (m^.stGammas)

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
                    <> help "One of: phis, psis, lambdas, omegas, gammas, influences, edge-mixtures"
                     )
    <*> strOption    ( long "sweeps"
                    <> short 's'
                    <> value "sweeps"
                    <> metavar "DIR"
                    <> help "The directory of sweeps to dump"
                     )
    <*> option       ( long "sweep-n"
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
        <> progDesc "Dump distributions from an citation influence model sweep"
        <> header "dump-ci - Dump distributions from an citation influence model sweep"
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
