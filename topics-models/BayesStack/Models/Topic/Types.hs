{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.Types where

import Control.DeepSeq
import GHC.Generics
import Data.Serialize
import Data.Function
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

newtype Node = Node Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype Item = Item Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype Topic = Topic Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype NodeItem = NodeItem Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype Friendship = Friendship (Node, Node) deriving (Show, Generic, NFData)

instance Serialize Node
instance Serialize Item
instance Serialize Topic
instance Serialize NodeItem
instance Serialize Friendship

instance Eq Friendship where
  (Friendship (a,b)) == (Friendship (c,d)) = (a == c && b == d) || (a == d && b == c)
instance Enum Friendship where
  fromEnum (Friendship (a,b)) = let a' = min a b
                                    b' = max a b
                                in 2^32 * fromEnum a' + fromEnum b'
  toEnum n = let (na, nb) = n `quotRem` (2^32)
             in Friendship (toEnum na, toEnum nb)
instance Ord Friendship where
  compare = compare `on` fromEnum

otherFriend :: Node -> Friendship -> Maybe Node
otherFriend u (Friendship (a,b))
  | u == a     = Just b
  | u == b     = Just a
  | otherwise  = Nothing

isFriend :: Node -> Friendship -> Bool
isFriend u fs = isJust $ otherFriend u fs

getFriends :: [Friendship] -> Node -> [Node]
getFriends fs u = mapMaybe (otherFriend u) fs

setupNodeItems :: [(Node,Item)] -> Map NodeItem (Node, Item)
setupNodeItems nodeItems = M.fromList $ zipWith (\idx (n,i)->(NodeItem idx, (n,i))) [0..] nodeItems

