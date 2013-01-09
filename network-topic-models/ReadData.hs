{-# LANGUAGE PatternGuards #-}

module ReadData ( Term, NodeName
                , readEdges
                , readNodeItems
                , getLastSweep
                , readItemMap, readNodeMap
                ) where

import           BayesStack.Models.Topic.Types
import           BayesStack.Models.Topic.CitationInfluence

import qualified Data.Set as S
import           Data.Set (Set)
                 
import qualified Data.Map as M

import           Data.Maybe (mapMaybe)
import           Control.Applicative

import           Data.Char (isAlpha)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Read (decimal)
import           Data.Binary
import qualified Data.ByteString as BS
import           SerializeText ()
                 
import           System.FilePath ((</>))
import           System.Directory
import           Data.List                 

type Term = T.Text
type NodeName = T.Text

readEdges :: FilePath -> IO (Set (NodeName, NodeName))
readEdges fname =
    S.fromList . mapMaybe parseLine . T.lines <$> TIO.readFile fname
    where parseLine :: T.Text -> Maybe (NodeName, NodeName)
          parseLine l = case T.words l of
             [a,b]     -> Just (a, b)
             otherwise -> Nothing

readNodeItems :: Set Term -> FilePath -> IO (M.Map NodeName [Term])
readNodeItems stopWords fname =
    M.unionsWith (++) . map parseLine . T.lines <$> TIO.readFile fname
    where parseLine :: T.Text -> M.Map NodeName [Term]
          parseLine l = case T.words l of
             n:words ->
                 M.singleton n
                 $ filter (\word->T.length word > 4)
                 $ map (T.filter isAlpha)
                 $ filter (`S.notMember` stopWords) words
             otherwise -> M.empty

getLastSweep :: FilePath -> IO FilePath
getLastSweep sweepsDir =
    (sweepsDir </>) . last . sort . filter (".state" `isSuffixOf`)
    <$> getDirectoryContents sweepsDir
    
readItemMap :: FilePath -> IO (M.Map Item Term)                 
readItemMap sweepsDir =
    decodeFile $ sweepsDir </> "item-map"

readNodeMap :: FilePath -> IO (M.Map Node NodeName)              
readNodeMap sweepsDir =
    decodeFile $ sweepsDir </> "node-map"

