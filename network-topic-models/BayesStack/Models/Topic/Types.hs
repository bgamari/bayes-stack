{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.Types where

import Control.DeepSeq
import GHC.Generics
import Data.Binary
import Data.Function
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

newtype Node = Node Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype Item = Item Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype Topic = Topic Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype NodeItem = NodeItem Int deriving (Show, Eq, Ord, Enum, Generic, NFData)
newtype Edge = Edge (Node, Node) deriving (Show, Generic, NFData)

instance Binary Node
instance Binary Item
instance Binary Topic
instance Binary NodeItem
instance Binary Edge

instance Eq Edge where
  (Edge (a,b)) == (Edge (c,d)) = (a == c && b == d) || (a == d && b == c)
instance Enum Edge where
  fromEnum (Edge (a,b)) = let a' = min a b
                              b' = max a b
                          in 2^32 * fromEnum a' + fromEnum b'
  toEnum n = let (na, nb) = n `quotRem` (2^32)
             in Edge (toEnum na, toEnum nb)
instance Ord Edge where
  compare = compare `on` fromEnum

otherFriend :: Node -> Edge -> Maybe Node
otherFriend u (Edge (a,b))
  | u == a     = Just b
  | u == b     = Just a
  | otherwise  = Nothing

isFriend :: Node -> Edge -> Bool
isFriend u fs = isJust $ otherFriend u fs

getFriends :: [Edge] -> Node -> [Node]
getFriends fs u = mapMaybe (otherFriend u) fs

setupNodeItems :: [(Node,Item)] -> Map NodeItem (Node, Item)
setupNodeItems nodeItems = M.fromList $ zipWith (\idx (n,i)->(NodeItem idx, (n,i))) [0..] nodeItems
