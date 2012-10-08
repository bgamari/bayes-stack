{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Data.Foldable
import           Data.List
import           Data.Function (on)
import           Options.Applicative

import qualified Data.Map as M
import qualified Data.ByteString as BS

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat
import           Data.Serialize

import           System.FilePath ((</>))                 
import           Text.Printf

import           BayesStack.Models.Topic.CitationInfluence
import           SerializeText
import           ReadData
import           FormatMultinom                 
                 
data Opts = Opts { nElems   :: Int
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
readDumper "phis"   = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Topic n)->"Topic "<>decimal n) showItem (nElems opts) (stPhis m)

readDumper "psis"   = Just $ \opts nd m showItem showNode ->
    formatMultinoms (\(Citing n)->showNode n) showB (nElems opts) (stPsis m)

readDumper "lambdas"= Just $ \opts nd m showItem showNode ->
    formatMultinoms showB showB (nElems opts) (stLambdas m)

readDumper "omegas" = Just $ \opts nd m showItem showNode ->
    formatMultinoms showB showB (nElems opts) (stOmegas m)

readDumper "gammas" = Just $ \opts nd m showItem showNode ->
    formatMultinoms showB showB (nElems opts) (stGammas m)
    
readDumper "influences" = Just $ \opts nd m showItem showNode ->
    let formatProb = formatRealFloat Exponent (Just 3) . realToFrac
        formatInfluences u =
            foldMap (\(Cited n,p)->"  " <> showNode n <> "\t" <> formatProb p <> "\n")
            $ sortBy (flip (compare `on` snd))
            $ M.assocs $ influence nd m u
    in foldMap (\u@(Citing u')->"\n" <> showB u' <> ":\n" <> formatInfluences u)
       $ M.keys $ stGammas m

readDumper _        = Nothing

opts = Opts
    <$> option       ( long "n-elems"
                    <> short 'n'
                    <> value 30
                    <> metavar "N"
                    <> help "Number of elements to output from each distribution"
                     )
    <*> argument readDumper
                     ( metavar "STR"
                     )
    <*> strOption    ( long "sweeps"
                    <> short 's'
                    <> value "sweeps"
                    <> metavar "DIR"
                    <> help "The directory of sweeps to dump"
                     )
    <*> option       ( long "sweep-n"
                    <> short 'N'
                    <> reader (Just . auto)
                    <> value Nothing
                    <> metavar "N"
                    <> help "The sweep number to dump"
                     )

readItemMap :: FilePath -> IO (M.Map Item Term)                 
readItemMap sweepsDir =
    (either error id . runGet get) <$> BS.readFile (sweepsDir </> "item-map")

readSweep :: FilePath -> IO MState
readSweep fname = (either error id . runGet get) <$> BS.readFile fname

readNetData :: FilePath -> IO NetData
readNetData fname = (either error id . runGet get) <$> BS.readFile fname

main = do
    args <- execParser $ info (helper <*> opts) 
         ( fullDesc 
        <> progDesc "Dump distributions from an citation influence model sweep"
        <> header "dump-ci - Dump distributions from an citation influence model sweep"
         )

    nd <- readNetData $ sweepDir args </> "data"
    itemMap <- readItemMap $ sweepDir args
    m <- case sweepNum args of
             Nothing -> readSweep =<< getLastSweep (sweepDir args)
             Just n  -> readSweep $ sweepDir args </> printf "%05d.state" n

    let showItem = showB . (itemMap M.!)
        showNode = showB
    TL.putStr $ TB.toLazyText $ dumper args args nd m showItem showNode
