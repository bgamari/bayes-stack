{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Control.Applicative

import qualified Data.Map as M
import qualified Data.ByteString as BS

import qualified Data.Text.Lazy.IO as TL
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Data.Serialize

import           BayesStack.Models.Topic.LDA
import           ReadData
import           FormatMultinom                 
                 
readItemMap :: IO (M.Map Item Term)                 
readItemMap =
    (either error id . runGet get) <$> BS.readFile "sweeps/node-map"

readSweep :: FilePath -> IO MState
readSweep fname = (either error id . runGet get) <$> BS.readFile fname

readNetData :: FilePath -> IO NetData
readNetData fname = (either error id . runGet get) <$> BS.readFile fname

dumpPhis :: M.Map Item Term -> MState -> TB.Builder
dumpPhis itemMap m =
    formatMultinoms (\(Topic n)->"Topic "<>decimal n)
                    (TB.fromString . show . (itemMap M.!))
                    30
                    (stPhis m)

dumpThetas :: MState -> TB.Builder
dumpThetas m =
    formatMultinoms (\(Node n)->"Node "<>decimal n)
                    (TB.fromString . show)
                    30
                    (stThetas m)

main = do
    d <- readNetData "sweeps/data"
    itemMap <- readItemMap
    m <- readSweep "sweeps/00010"
    --TL.putStr $ TB.toLazyText $ dumpPhis itemMap m
    TL.putStr $ TB.toLazyText $ dumpThetas m

instance Serialize T.Text where
     put = put . TE.encodeUtf8
     get = TE.decodeUtf8 <$> get
