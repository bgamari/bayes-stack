{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module SharedTaste ( STData(..)
                   , Node(..), Item(..), Topic(..)
                   , Friendship(..), otherFriend, isFriend, getFriends
                   , STModel(..), ItemUnit
                   , model, likelihood
                   , STModelState (..), getModelState
                   ) where

import Prelude hiding (mapM)

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.Set (Set)
import qualified Data.Set as S

import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Function (on)
import Data.Maybe (mapMaybe, isJust)

import Control.Monad (liftM)
import Data.Random
import Data.Random.Sequence
import Data.Number.LogFloat

import BayesStack.Core
import BayesStack.Categorical
import BayesStack.DirMulti
import BayesStack.TupleEnum

import Control.Monad (when)
import Control.Monad.IO.Class

import Data.Serialize
import GHC.Generics

data STData = STData { stAlphaPsi :: Double
                     , stAlphaLambda :: Double
                     , stAlphaPhi :: Double
                     , stNodes :: Set Node
                     , stFriendships :: Set Friendship
                     , stItems :: Set Item
                     , stTopics :: Set Topic
                     , stNodeItems :: Seq (Node, Item)
                     }
               deriving (Show, Eq)

newtype Node = Node Int deriving (Show, Eq, Ord, Enum, Generic)
newtype Item = Item Int deriving (Show, Eq, Ord, Enum, Generic)
newtype Topic = Topic Int deriving (Show, Eq, Ord, Enum, Generic)
newtype NodeItem = NodeItem Int deriving (Show, Eq, Ord, Enum, Generic)
newtype Friendship = Friendship (Node, Node) deriving (Show, Generic)

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

data STModel = STModel { mNodes :: Set Node
                       , mTopics :: Set Topic
                       , mItems :: Set Item
                       , mFriendships :: Set Friendship
                       , mNodeItems :: EnumMap NodeItem (Node, Item)
                       , mPsis :: SharedEnumMap Node (DirMulti Node)
                       , mLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                       , mPhis :: SharedEnumMap Topic (DirMulti Item)
                       , mFs :: SharedEnumMap NodeItem Node
                       , mTs :: SharedEnumMap NodeItem Topic
                       }

data STModelState = STModelState { msNodes :: Set Node
                                 , msTopics :: Set Topic
                                 , msItems :: Set Item
                                 , msFriendships :: Set Friendship
                                 , msNodeItems :: EnumMap NodeItem (Node, Item)
                                 , msPsis :: EnumMap Node (DirMulti Node)
                                 , msLambdas :: EnumMap Friendship (DirMulti Topic)
                                 , msPhis :: EnumMap Topic (DirMulti Item)
                                 , msFs :: EnumMap NodeItem Node
                                 , msTs :: EnumMap NodeItem Topic
                                 , msLogLikelihood :: Double
                                 } deriving (Show, Generic)
instance Serialize STModelState

data ItemUnit = ItemUnit { iuTopics :: Set Topic
                         , iuNodeItem :: NodeItem
                         , iuFriends :: Set Node
                         , iuN :: Node
                         , iuF :: Shared Node
                         , iuT :: Shared Topic
                         , iuX :: Item
                         , iuPsi :: Shared (DirMulti Node)
                         , iuLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                         , iuPhis :: SharedEnumMap Topic (DirMulti Item)
                         }

model :: STData -> ModelMonad (Seq ItemUnit, STModel)
model d =
  do let STData {stTopics=topics, stNodes=nodes, stItems=items, stNodeItems=nodeItems} = d
         STData {stFriendships=friendships} = d
         nis :: EnumMap NodeItem (Node,Item)
         nis = EM.fromList $ zipWith (\idx (n,i)->(NodeItem idx, (n,i))) [0..] (toList nodeItems)
         friends :: EnumMap Node (Set Node)
         --friends = map (\n->(n, S.map (otherFriend n) $ S.filter (isFriend n) friendships)) nodes
         friends = EM.fromList $ map (\n->(n, S.fromList $ mapMaybe (otherFriend n) $ S.toList friendships)) $ S.toList nodes
     psis <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaPsi d) (S.toList nodes)
     lambdas <- newSharedEnumMap (S.toList friendships) $ \n ->
       return $ symDirMulti (stAlphaLambda d) (S.toList topics)
     phis <- newSharedEnumMap (S.toList topics) $ \t ->
       return $ symDirMulti (stAlphaPhi d) (S.toList items)
     ts <- newSharedEnumMap (EM.keys nis) $ \ni ->
       liftRVar $ randomElementT $ SQ.fromList $ S.toList topics
     fs <- newSharedEnumMap (EM.keys nis) $ \ni ->
       let (n,i) = nis EM.! ni
       in liftRVar $ randomElementT $ SQ.fromList $ S.toList $ friends EM.! n
  
     itemUnits <- forM (EM.keys nis) $ \ni ->
       do let t = ts EM.! ni
              f = fs EM.! ni
              (n,x) = nis EM.! ni
          let unit = ItemUnit { iuTopics = topics
                              , iuNodeItem = ni
                              , iuFriends = friends EM.! n
                              , iuN = n
                              , iuF = f
                              , iuT = t
                              , iuX = x
                              , iuPsi = psis EM.! n
                              , iuLambdas = EM.filterWithKey (\k _->isFriend n k) lambdas
                              , iuPhis = phis
                              }
          t' <- getShared t
          f' <- getShared f
          guSet unit (t',f')
          return unit
     let model = STModel { mNodes = nodes
                         , mTopics = topics
                         , mItems = items
                         , mFriendships = friendships
                         , mNodeItems = nis
                         , mPsis = psis
                         , mLambdas = lambdas
                         , mPhis = phis
                         , mFs = fs
                         , mTs = ts }
     return (SQ.fromList itemUnits, model)

likelihood :: STModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ mNodeItems model) $ \(ni, (n,x)) ->
       do t <- getShared $ mTs model EM.! ni 
          f <- getShared $ mFs model EM.! ni 
          psi <- getShared $ mPsis model EM.! n
          lambda <- getShared $ mLambdas model EM.! Friendship (n,f)
          phi <- getShared $ mPhis model EM.! t
          return $ Product $ logFloat (prob psi f)
                           * logFloat (prob lambda t)
                           * logFloat (prob phi x)
     return $ getProduct $ mconcat a

instance GibbsUpdateUnit ItemUnit where
  type GUValue ItemUnit = (Topic, Node)
  guProb unit (t,f) =
    do phi <- getShared $ iuPhis unit EM.! t 
       lambda <- getShared $ iuLambdas unit EM.! Friendship (iuN unit, f)
       return $ probPretend lambda t * probPretend phi (iuX unit) 
  
  guDomain unit = return $ do t <- S.toList $ iuTopics unit
                              f <- S.toList $ iuFriends unit
                              return (t,f)
  
  guUnset unit =
    do t <- getShared $ iuT unit 
       f <- getShared $ iuF unit 
       let x = iuX unit
           psi = iuPsi unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       psi `updateShared` decDirMulti f
       lambda `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
  
  guSet unit (t,f) =
    do iuT unit `setShared` t
       iuF unit `setShared` f
       let x = iuX unit
           psi = iuPsi unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       psi `updateShared` incDirMulti f
       lambda `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x

getSharedEnumMap :: Enum a => SharedEnumMap a b -> ModelMonad (EnumMap a b)
getSharedEnumMap = liftM EM.fromList . mapM (\(k,v)->do v' <- getShared v
                                                     return (k,v')
                                         ) . EM.toList

getModelState :: STModel -> ModelMonad STModelState
getModelState model =
  do psis <- getSharedEnumMap $ mPsis model
     lambdas <- getSharedEnumMap $ mLambdas model
     phis <- getSharedEnumMap $ mPhis model
     fs <- getSharedEnumMap $ mFs model
     ts <- getSharedEnumMap $ mTs model
     l <- likelihood model
     return $ STModelState { msNodes = mNodes model
                           , msTopics = mTopics model
                           , msItems = mItems model
                           , msFriendships = mFriendships model
                           , msNodeItems = mNodeItems model
                           , msPsis = psis
                           , msLambdas = lambdas
                           , msPhis = phis
                           , msFs = fs
                           , msTs = ts
                           , msLogLikelihood = logFromLogFloat l
                           }
