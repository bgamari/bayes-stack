{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.SharedTaste
  ( -- * Primitives
    STData(..)
  , Node(..), Item(..), Topic(..)
  , NodeItem, setupNodeItems
  , Friendship(..), otherFriend, isFriend, getFriends
  -- * Initialization
  , ModelInit
  , randomInitialize
  -- * Model
  , STModel(..), ItemUnit
  , model, likelihood
  , STModelState (..), getModelState
  , sortTopics
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
import Data.List (sortBy)

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
                     , stNodeItems :: EnumMap NodeItem (Node, Item)
                     }
              deriving (Show, Eq, Generic)
instance Serialize STData

data ItemVars = ItemVars { ivF :: Node
                         , ivT :: Topic
                         }
                deriving (Show, Eq, Generic)
instance Serialize ItemVars

data STModel = STModel { mData :: STData
                       , mPsis :: SharedEnumMap Node (DirMulti Node)
                       , mLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                       , mPhis :: SharedEnumMap Topic (DirMulti Item)
                       , mVars :: SharedEnumMap NodeItem ItemVars
                       , mSortedTopics :: SharedEnumMap Item [Topic]
                       }

data STModelState = STModelState { msData :: STData
                                 , msPsis :: EnumMap Node (DirMulti Node)
                                 , msLambdas :: EnumMap Friendship (DirMulti Topic)
                                 , msPhis :: EnumMap Topic (DirMulti Item)
                                 , msVars :: EnumMap NodeItem ItemVars
                                 , msLogLikelihood :: Double
                                 }
                    deriving (Show, Generic)
instance Serialize STModelState

data ItemUnit = ItemUnit { iuModel :: STModel
                         , iuTopics :: Set Topic
                         , iuNodeItem :: NodeItem
                         , iuFriends :: Set Node
                         , iuN :: Node
                         , iuVars :: Shared ItemVars
                         , iuX :: Item
                         , iuPsi :: Shared (DirMulti Node)
                         , iuLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                         , iuPhis :: SharedEnumMap Topic (DirMulti Item)
                         , iuState :: Shared GibbsUpdateState
                         }

type ModelInit = EnumMap NodeItem ItemVars

randomInitialize' :: STData -> ModelInit -> RVar ModelInit
randomInitialize' d init = 
  let unset = EM.keysSet (stNodeItems d) `ES.difference` EM.keysSet init
      topics = S.toList $ stTopics d
      randomInit :: NodeItem -> RVar ModelInit
      randomInit ni = do t <- randomElement topics
                         let (n,_) = stNodeItems d EM.! ni
                             friends = getFriends (S.toList $ stFriendships d) n
                         f <- randomElement friends
                         return $ EM.singleton ni $ ItemVars f t
  in liftM mconcat $ forM (ES.toList unset) randomInit

randomInitialize :: STData -> RVar ModelInit
randomInitialize = (flip randomInitialize') EM.empty

model :: STData -> ModelInit -> ModelMonad (Seq ItemUnit, STModel)
model d init =
  do let STData {stTopics=topics, stNodes=nodes, stItems=items, stNodeItems=nis} = d
         STData {stFriendships=friendships} = d
         friends :: EnumMap Node (Set Node)
         friends = foldMap (\n->EM.singleton n $ S.fromList $ getFriends (S.toList friendships) n) nodes
     psis <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaPsi d) (S.toList nodes)
     lambdas <- newSharedEnumMap (S.toList friendships) $ \n ->
       return $ symDirMulti (stAlphaLambda d) (S.toList topics)
     phis <- newSharedEnumMap (S.toList topics) $ \t ->
       return $ symDirMulti (stAlphaPhi d) (S.toList items)

     ivs <- newSharedEnumMap (EM.keys nis) $ \ni -> return $ init EM.! ni
  
     sortedTopics <- newSharedEnumMap (S.toList items) $ \x -> return $ S.toList topics

     let model = STModel { mData = d
                         , mPsis = psis
                         , mLambdas = lambdas
                         , mPhis = phis
                         , mVars = ivs
                         , mSortedTopics = sortedTopics
                         }

     itemUnits <- forM (EM.toList ivs) $ \(ni,iv) ->
       do state <- newGibbsUpdateState
          let (n,x) = nis EM.! ni
              unit = ItemUnit { iuModel = model
                              , iuTopics = topics
                              , iuNodeItem = ni
                              , iuFriends = friends EM.! n
                              , iuN = n
                              , iuVars = iv
                              , iuX = x
                              , iuPsi = psis EM.! n
                              , iuLambdas = EM.filterWithKey (\k _->isFriend n k) lambdas
                              , iuPhis = phis
                              , iuState = state
                              }
          getShared iv >>= guSet unit
          return unit
     return (SQ.fromList itemUnits, model)

sortTopics :: STModel -> ModelMonad ()
sortTopics model =
  forM_ (EM.toList $ mSortedTopics model) $ \(x,topics)->do
    d <- getShared topics
    weights <- forM d $ \t->do phi <- getShared $ mPhis model EM.! t
                               return $ prob phi x
    setShared topics $ map snd $ sortBy (flip (compare `on` fst)) $ zip weights d

likelihood :: STModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ stNodeItems $ mData model) $ \(ni, (n,x)) ->
       do ItemVars f t <- getShared $ mVars model EM.! ni 
          psi <- getShared $ mPsis model EM.! n
          lambda <- getShared $ mLambdas model EM.! Friendship (n,f)
          phi <- getShared $ mPhis model EM.! t
          return $ Product $ logFloat (prob psi f)
                           * logFloat (prob lambda t)
                           * logFloat (prob phi x)
     return $ getProduct $ mconcat a

instance GibbsUpdateUnit ItemUnit where
  type GUValue ItemUnit = ItemVars
  guProb unit (ItemVars f t) =
    do psi <- getShared $ iuPsi unit
       phi <- getShared $ iuPhis unit EM.! t 
       lambda <- getShared $ iuLambdas unit EM.! Friendship (iuN unit, f)
       return $ prob psi f * prob lambda t * prob phi (iuX unit) 
  
  guDomain unit = do topics <- getShared $ mSortedTopics (iuModel unit) EM.! iuX unit
                     return $ do t <- topics
                                 f <- S.toList $ iuFriends unit
                                 return $ ItemVars f t
  
  guUnset unit =
    do ItemVars f t <- getShared $ iuVars unit
       let x = iuX unit
           psi = iuPsi unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       psi `updateShared` decDirMulti f
       lambda `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
       return $ ItemVars f t
  
  guSet unit iv@(ItemVars f t) =
    do iuVars unit `setShared` iv
       let x = iuX unit
           psi = iuPsi unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       psi `updateShared` incDirMulti f
       lambda `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x

  guState = iuState

getModelState :: STModel -> ModelMonad STModelState
getModelState model =
  do psis <- getSharedEnumMap $ mPsis model
     lambdas <- getSharedEnumMap $ mLambdas model
     phis <- getSharedEnumMap $ mPhis model
     vars <- getSharedEnumMap $ mVars model
     l <- likelihood model
     return $ STModelState { msData = mData model
                           , msPsis = psis
                           , msLambdas = lambdas
                           , msPhis = phis
                           , msVars = vars
                           , msLogLikelihood = logFromLogFloat l
                           }

