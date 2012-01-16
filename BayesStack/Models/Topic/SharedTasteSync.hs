{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.SharedTasteSync
  ( -- * Primitives
    STData(..)
  , Node(..), Item(..), Topic(..)
  , NodeItem, setupNodeItems
  , Friendship(..), otherFriend, isFriend, getFriends
  -- * Initialization
  , ModelInit
  , randomInitialize, initialize
  -- * Model
  , STModel(..), ItemUnit(..)
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

import qualified Data.EnumSet as ES

import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Function (on)
import Data.Maybe (mapMaybe, isJust)
import Data.Tuple

import Control.Monad (liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Random
import Data.Random.List
import Data.Number.LogFloat

import BayesStack.Core
import BayesStack.Categorical
import BayesStack.DirMulti
import BayesStack.TupleEnum
import BayesStack.Models.Topic.Types

import Control.Monad (when)
import Control.Monad.IO.Class

import Data.Serialize (Serialize)
import GHC.Generics

data STData = STData { stAlphaPsi :: Double
                     , stAlphaLambda :: Double
                     , stAlphaPhi :: Double
                     , stNodes :: Set Node
                     , stFriendships :: Set Friendship
                     , stItems :: Set Item
                     , stTopics :: Set Topic
                     , stNodeItems :: EnumMap NodeItem (Node, Item)
                     }
               deriving (Show, Eq, Generic)
instance Serialize STData

data STModel = STModel { mData :: STData
                       , mPsis :: SharedEnumMap Node (DirMulti Node)
                       , mLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                       , mPhis :: SharedEnumMap Topic (DirMulti Item)
                       , mFs :: SharedEnumMap NodeItem Node
                       , mTs :: SharedEnumMap NodeItem Topic
                       }

data STModelState = STModelState { msData :: STData
                                 , msPsis :: EnumMap Node (DirMulti Node)
                                 , msLambdas :: EnumMap Friendship (DirMulti Topic)
                                 , msPhis :: EnumMap Topic (DirMulti Item)
                                 , msFs :: EnumMap NodeItem Node
                                 , msTs :: EnumMap NodeItem Topic
                                 , msLogLikelihood :: Double
                                 } deriving (Show, Generic)
instance Serialize STModelState

data ItemUnit = ItemUnit { iuModel :: STModel
                         , iuNodeItem :: NodeItem
                         , iuFriends :: Set Node
                         , iuN :: Node
                         , iuF :: Shared Node
                         , iuT :: Shared Topic
                         , iuX :: Item
                         , iuLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                         , iuPhis :: SharedEnumMap Topic (DirMulti Item)
                         }

type ModelInit = EnumMap NodeItem (Node, Topic)

randomInitialize' :: STData -> ModelInit -> RVar ModelInit
randomInitialize' d init = 
  let unset = EM.keysSet (stNodeItems d) `ES.difference` EM.keysSet init
      topics = S.toList $ stTopics d
      randomInit :: NodeItem -> RVar ModelInit
      randomInit ni = do t <- randomElement topics
                         let (n,_) = stNodeItems d EM.! ni
                             friends = getFriends (S.toList $ stFriendships d) n
                         f <- randomElement friends
                         return $ EM.singleton ni (f,t)
  in liftM mconcat $ forM (ES.toList unset) randomInit

randomInitialize, initialize :: STData -> RVar ModelInit
randomInitialize = (flip randomInitialize') EM.empty
initialize d =
  let STData {stTopics=topics, stNodes=nodes, stItems=items, stNodeItems=nodeItems} = d
      STData {stFriendships=friendships, stNodeItems=nis} = d
      nisInv :: EnumMap (Node,Item) [NodeItem]
      nisInv = EM.fromListWith (++) $ map (\(ni,(n,i))->((n,i),[ni])) $ EM.toList nis

      sharedTopics :: Friendship -> StateT (EnumMap Item Topic, EnumMap Friendship (Set Topic)) RVar ModelInit
      sharedTopics fs@(Friendship (a,b)) =
        let findItems n = S.fromList $ map snd $ filter (\(n',i)->n==n') $ toList nodeItems
            sharedItems = toList $ S.intersection (findItems a) (findItems b)
        in liftM mconcat $ forM sharedItems $ \x -> do
             (itemTopics,_) <- get
             t <- if x `EM.member` itemTopics
                    then return $ itemTopics EM.! x
                    else do (_, friendshipTopics) <- get
                            --let possTopics = EM.findWithDefault topics fs friendshipTopics
                            let possTopics = topics -- FIXME
                            t <- lift $ randomElement $ S.toList possTopics
                            modify $ \(a,b)->(EM.insert x t a, b)
                            return t
             modify $ \(a,b)->(a, EM.insertWith S.union fs (S.singleton t) b)
             return $ EM.fromList $ do ax <- nisInv EM.! (a,x)
                                       return (ax, (b,t))
                                 ++ do bx <- nisInv EM.! (b,x)
                                       return (bx, (a,t))
  in do a <- evalStateT (mapM sharedTopics $ S.toList friendships) (EM.empty, EM.empty)
        randomInitialize' d $ mconcat a

model :: STData -> ModelInit -> ModelMonad (Seq ItemUnit, STModel)
model d init =
  do let STData {stTopics=topics, stNodes=nodes, stItems=items, stNodeItems=nodeItems} = d
         STData {stFriendships=friendships, stNodeItems=nis} = d
         friends :: EnumMap Node (Set Node)
         --friends = map (\n->(n, S.map (otherFriend n) $ S.filter (isFriend n) friendships)) nodes
         friends = EM.fromList $ map (\n->(n, S.fromList $ getFriends (S.toList friendships) n))
                   $ S.toList nodes
     psis <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaPsi d) (S.toList $ friends EM.! n)
     lambdas <- newSharedEnumMap (S.toList friendships) $ \n ->
       return $ symDirMulti (stAlphaLambda d) (S.toList topics)
     phis <- newSharedEnumMap (S.toList topics) $ \t ->
       return $ symDirMulti (stAlphaPhi d) (S.toList items)
     fs <- newSharedEnumMap (EM.keys nis) $ \ni -> return $ fst $ init EM.! ni
     ts <- newSharedEnumMap (EM.keys nis) $ \ni -> return $ snd $ init EM.! ni
  
     let model = STModel { mData = d
                         , mPsis = psis
                         , mLambdas = lambdas
                         , mPhis = phis
                         , mFs = fs
                         , mTs = ts }

     itemUnits <- forM (EM.keys nis) $ \ni ->
       do let t = ts EM.! ni
              f = fs EM.! ni
              (n,x) = nis EM.! ni
          let unit = ItemUnit { iuModel = model
                              , iuNodeItem = ni
                              , iuFriends = friends EM.! n
                              , iuN = n
                              , iuF = f
                              , iuT = t
                              , iuX = x
                              , iuLambdas = EM.filterWithKey (\k _->isFriend n k) lambdas
                              , iuPhis = phis
                              }
          t' <- getShared t
          f' <- getShared f
          guSet unit (t',f')
          return unit
     return (SQ.fromList itemUnits, model)

likelihood :: STModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ stNodeItems $ mData model) $ \(ni, (n,x)) ->
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
    do psi <- getShared $ mPsis (iuModel unit) EM.! iuN unit
       phi <- getShared $ iuPhis unit EM.! t 
       lambda <- getShared $ iuLambdas unit EM.! Friendship (iuN unit, f)
       return $ prob psi f * prob lambda t * prob phi (iuX unit) 
  
  guDomain unit = return $ do t <- S.toList $ stTopics $ mData $ iuModel unit
                              f <- S.toList $ iuFriends unit
                              return (t,f)
  
  guUnset unit =
    do t <- getShared $ iuT unit 
       f <- getShared $ iuF unit 
       let x = iuX unit
           u = iuN unit
           m = iuModel unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       (mPsis m EM.! u) `updateShared` decDirMulti f
       (mPsis m EM.! f) `updateShared` decDirMulti u
       lambda `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
       return (t,f)
  
  guSet unit (t,f) =
    do iuT unit `setShared` t
       iuF unit `setShared` f
       let x = iuX unit
           u = iuN unit
           m = iuModel unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       (mPsis m EM.! u) `updateShared` incDirMulti f
       (mPsis m EM.! f) `updateShared` incDirMulti u
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
                           , msPsis = psis
                           , msLambdas = lambdas
                           , msPhis = phis
                           , msFs = fs
                           , msTs = ts
                           , msLogLikelihood = logFromLogFloat l
                           }

