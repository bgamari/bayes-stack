{-# LANGUAGE PatternGuards #-}

module ReadData ( Term
                , readEdges
                , readNodeItems
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

readEdges :: FilePath -> IO (Set (Node, Node))
readEdges fname =
    S.fromList . mapMaybe parseLine . T.lines <$> TIO.readFile fname
    where parseLine :: T.Text -> Maybe (Node, Node)
          parseLine l = case T.words l of
             [a,b] -> case (decimal a, decimal b) of
                          (Right (a',_), Right (b',_)) ->
                              Just (Node a', Node b')
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