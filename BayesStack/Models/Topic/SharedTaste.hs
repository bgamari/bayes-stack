{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.SharedTaste
  ( STData(..)
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
import BayesStack.Models.Topic.Types

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
              deriving (Show, Eq, Generic)
instance Serialize STData

data STModel = STModel { mData :: STData
                       , mNodeItems :: EnumMap NodeItem (Node, Item)
                       , mPsis :: SharedEnumMap Node (DirMulti Node)
                       , mLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                       , mPhis :: SharedEnumMap Topic (DirMulti Item)
                       , mFs :: SharedEnumMap NodeItem Node
                       , mTs :: SharedEnumMap NodeItem Topic
                       }

data STModelState = STModelState { msData :: STData
                                 , msNodeItems :: EnumMap NodeItem (Node, Item)
                                 , msPsis :: EnumMap Node (DirMulti Node)
                                 , msLambdas :: EnumMap Friendship (DirMulti Topic)
                                 , msPhis :: EnumMap Topic (DirMulti Item)
                                 , msFs :: EnumMap NodeItem Node
                                 , msTs :: EnumMap NodeItem Topic
                                 , msLogLikelihood :: Double
                                 }
                    deriving (Show, Generic)
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
         friends = EM.fromList $ map (\n->(n, S.fromList $ getFriends (S.toList friendships) n)) $ S.toList nodes
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
     let model = STModel { mData = d
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
    do psi <- getShared $ iuPsi unit
       phi <- getShared $ iuPhis unit EM.! t 
       lambda <- getShared $ iuLambdas unit EM.! Friendship (iuN unit, f)
       return $ probPretend psi f * probPretend lambda t * probPretend phi (iuX unit) 
  
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
       return (t,f)
  
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

getModelState :: STModel -> ModelMonad STModelState
getModelState model =
  do psis <- getSharedEnumMap $ mPsis model
     lambdas <- getSharedEnumMap $ mLambdas model
     phis <- getSharedEnumMap $ mPhis model
     fs <- getSharedEnumMap $ mFs model
     ts <- getSharedEnumMap $ mTs model
     l <- likelihood model
     return $ STModelState { msData = mData model
                           , msNodeItems = mNodeItems model
                           , msPsis = psis
                           , msLambdas = lambdas
                           , msPhis = phis
                           , msFs = fs
                           , msTs = ts
                           , msLogLikelihood = logFromLogFloat l
                           }

