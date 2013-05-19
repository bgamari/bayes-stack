{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Data.Foldable
import           Data.List
import           Data.Function (on)
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

import           BayesStack.Models.Topic.CitationInfluence
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
readDumper "phis"   = Just $ \opts nd m showItem showNode ->
    formatMultinoms showTopic showItem (nElems opts) (stPhis m)

readDumper "psis"   = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Citing n)->showNode n) showB (nElems opts) (stPsis m)

readDumper "lambdas"= Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Cited n)->showNode n) showB (nElems opts) (stLambdas m)

readDumper "omegas" = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Citing n)->showNode n) showB (nElems opts) (stOmegas m)

readDumper "gammas" = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Citing n)->showNode n) showB (nElems opts) (stGammas m)

readDumper "influences" = Just $ \opts nd m showItem showNode ->
    let formatInfluences u =
            foldMap (\(Cited n,p)->"\t" <> showNode n <> "\t" <> formatProb p <> "\n")
            $ sortBy (flip (compare `on` snd))
            $ M.assocs $ influence nd m u
    in foldMap (\u@(Citing u')->"\n" <> showNode u' <> "\n" <> formatInfluences u)
       $ M.keys $ stGammas m

readDumper "edge-mixtures" = Just $ \opts nd m showItem showNode ->
    let showArc (Arc (Citing d) (Cited c)) = showNode d <> " -> " <> showNode c
        formatMixture a =
            let ps = sortBy (flip compare `on` snd)
                   $ map (\t->(t, arcTopicMixture nd m a t))
                   $ S.toList $ dTopics nd
                norm = Numeric.Log.sum $ map snd ps
            in foldMap (\(t,p)->"\t" <> showTopic t <> "\t" <> formatProb p <> "\n")
               $ maybe id take (nElems opts)
               $ map (\(t,p)->(t, p / norm)) ps
    in foldMap (\a->"\n" <> showArc a <> "\n" <> formatMixture a)
       $ S.toList $ dArcs nd

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
