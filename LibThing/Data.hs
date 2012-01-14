{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LibThing.Data where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
import Data.Map (Map)

import BayesStack.Models.Topic.Types
import BayesStack.UniqueKey

import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Char (toLower)
import Data.Hashable
import Control.Monad
import Text.CSV

newtype Group = Group Int deriving (Show, Ord, Eq, Enum)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads 

readFriendships :: IO [Friendship]
readFriendships = 
  do f' <- parseCSVFromFile "edges_with_profile.csv"
     let csv' = either (error . show) id f'
     return $ mapMaybe (\rec -> do when (length rec < 10) Nothing
                                   a <- maybeRead $ rec !! 8
                                   b <- maybeRead $ rec !! 9
                                   return $ Friendship (Node a, Node b))
                $ tail csv'

readTags :: IO ([(Node, Item)], Map Item String)
readTags =
  do f <- parseCSVFromFile "tags_for_users.csv"
     let csv = either (error . show) id f
         records = mapMaybe (\rec -> do a <- maybeRead $ rec !! 0
                                        return (a, map toLower $ rec !! 6))
                   $ tail csv
         (userTags, wordMap) = runUniqueKey $ forM records $ \(n,i) -> do k <- uniqueKey Item i
                                                                          return (Node n, k)

     return (userTags, wordMap)

readUserGroups :: IO [(Node, Group)]
readUserGroups =
  do csv <- liftM (either (error . show) id) $ parseCSVFromFile "r_usergroups_members.csv"
     return $ mapMaybe (\rec -> do u <- maybeRead $ rec !! 0
                                   g <- maybeRead $ rec !! 2
                                   return (Node u, Group g))
              $ tail csv

getGroups :: IO (EnumMap Group [Node])
getGroups =
  do ugs <- readUserGroups
     return $ EM.fromListWith (++) $ map (\(n,g)->(g,[n])) ugs

