{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.SharedTasteOwn
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

data STData = STData { stAlphaGamma :: [(Bool,Double)]
                     , stAlphaOmega :: Double
                     , stAlphaPsi :: Double
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
                       , mGammas :: SharedEnumMap Node (DirMulti Bool)
                       , mOmegas :: SharedEnumMap Node (DirMulti Topic)
                       , mPsis :: SharedEnumMap Node (DirMulti Node)
                       , mLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                       , mPhis :: SharedEnumMap Topic (DirMulti Item)
                       , mSs :: SharedEnumMap NodeItem Bool
                       , mFs :: SharedEnumMap NodeItem Node
                       , mTs :: SharedEnumMap NodeItem Topic
                       }

data STModelState = STModelState { msData :: STData
                                 , msNodeItems :: EnumMap NodeItem (Node, Item)
                                 , msGammas :: EnumMap Node (DirMulti Bool)
                                 , msOmegas :: EnumMap Node (DirMulti Topic)
                                 , msPsis :: EnumMap Node (DirMulti Node)
                                 , msLambdas :: EnumMap Friendship (DirMulti Topic)
                                 , msPhis :: EnumMap Topic (DirMulti Item)
                                 , msSs :: EnumMap NodeItem Bool
                                 , msFs :: EnumMap NodeItem Node
                                 , msTs :: EnumMap NodeItem Topic
                                 , msLogLikelihood :: Double
                                 }
                    deriving (Show, Generic)
instance Serialize STModelState

data ItemUnit = ItemUnit { iuModel :: STModel
                         , iuNodeItem :: NodeItem
                         , iuFriends :: Set Node
                         , iuN :: Node
                         , iuS :: Shared Bool
                         , iuF :: Shared Node
                         , iuT :: Shared Topic
                         , iuX :: Item
                         , iuGamma :: Shared (DirMulti Bool)
                         , iuOmega :: Shared (DirMulti Topic)
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
     gammas <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ dirMulti (stAlphaGamma d) [True, False]
     omegas <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaOmega d) (S.toList topics)
     psis <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaPsi d) (S.toList nodes)
     lambdas <- newSharedEnumMap (S.toList friendships) $ \n ->
       return $ symDirMulti (stAlphaLambda d) (S.toList topics)
     phis <- newSharedEnumMap (S.toList topics) $ \t ->
       return $ symDirMulti (stAlphaPhi d) (S.toList items)
     ss <- newSharedEnumMap (EM.keys nis) $ \ni ->
       liftRVar $ randomElementT $ SQ.fromList [True,False]
     ts <- newSharedEnumMap (EM.keys nis) $ \ni ->
       liftRVar $ randomElementT $ SQ.fromList $ S.toList topics
     fs <- newSharedEnumMap (EM.keys nis) $ \ni ->
       let (n,i) = nis EM.! ni
       in liftRVar $ randomElementT $ SQ.fromList $ S.toList $ friends EM.! n
  
     let model = STModel { mData = d
                         , mNodeItems = nis
                         , mGammas = gammas
                         , mOmegas = omegas
                         , mPsis = psis
                         , mLambdas = lambdas
                         , mPhis = phis
                         , mSs = ss
                         , mFs = fs
                         , mTs = ts }

     itemUnits <- forM (EM.keys nis) $ \ni ->
       do let (n,x) = nis EM.! ni
              s = ss EM.! ni
              t = ts EM.! ni
              f = fs EM.! ni
          let unit = ItemUnit { iuModel = model
                              , iuNodeItem = ni
                              , iuFriends = friends EM.! n
                              , iuN = n
                              , iuS = s
                              , iuF = f
                              , iuT = t
                              , iuX = x
                              , iuGamma = gammas EM.! n
                              , iuOmega = omegas EM.! n
                              , iuPsi = psis EM.! n
                              , iuLambdas = EM.filterWithKey (\k _->isFriend n k) lambdas
                              , iuPhis = phis
                              }
          s' <- getShared s
          t' <- getShared t
          f' <- getShared f
          guSet unit (s',t',f')
          return unit
     return (SQ.fromList itemUnits, model)

likelihood :: STModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ mNodeItems model) $ \(ni, (n,x)) ->
       do t <- getShared $ mTs model EM.! ni 
          f <- getShared $ mFs model EM.! ni 
          s <- getShared $ mSs model EM.! ni 
          gamma <- getShared $ mGammas model EM.! n
          omega <- getShared $ mOmegas model EM.! n
          psi <- getShared $ mPsis model EM.! n
          lambda <- getShared $ mLambdas model EM.! Friendship (n,f)
          phi <- getShared $ mPhis model EM.! t
          if s then return $ Product $ logFloat (prob gamma s)
                                     * logFloat (prob psi f)
                                     * logFloat (prob lambda t)
                                     * logFloat (prob phi x)
               else return $ Product $ logFloat (prob gamma s)
                                     * logFloat (prob omega t)
                                     * logFloat (prob phi x)
     return $ getProduct $ mconcat a

instance GibbsUpdateUnit ItemUnit where
  type GUValue ItemUnit = (Bool, Topic, Node)
  guProb unit (s,t,f) =
    do gamma <- getShared $ iuGamma unit
       omega <- getShared $ iuOmega unit
       psi <- getShared $ iuPsi unit
       phi <- getShared $ iuPhis unit EM.! t 
       lambda <- getShared $ iuLambdas unit EM.! Friendship (iuN unit, f)
       if s then return $ probPretend gamma s * probPretend psi f * probPretend lambda t * probPretend phi (iuX unit) 
            else return $ probPretend gamma s * probPretend omega t * probPretend phi (iuX unit)
  
  guDomain unit = return $ (do t <- S.toList $ stTopics $ mData $ iuModel unit
                               f <- S.toList $ iuFriends unit
                               return (True,t,f))
                        ++ (do t <- S.toList $ stTopics $ mData $ iuModel unit
                               let f = head $ S.toList $ iuFriends unit
                               return (False,t,f))
  
  guUnset unit =
    do s <- getShared $ iuS unit 
       t <- getShared $ iuT unit 
       f <- getShared $ iuF unit 
       let x = iuX unit
           gamma = iuGamma unit
           omega = iuOmega unit
           psi = iuPsi unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       gamma `updateShared` decDirMulti s
       if s then do psi `updateShared` decDirMulti f
                    lambda `updateShared` decDirMulti t
            else do omega `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
       return (s,t,f)
  
  guSet unit (s,t,f) =
    do iuS unit `setShared` s
       iuT unit `setShared` t
       iuF unit `setShared` f
       let x = iuX unit
           gamma = iuGamma unit
           omega = iuOmega unit
           psi = iuPsi unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       gamma `updateShared` incDirMulti s
       if s then do psi `updateShared` incDirMulti f
                    lambda `updateShared` incDirMulti t
            else do omega `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x

getModelState :: STModel -> ModelMonad STModelState
getModelState model =
  do gammas <- getSharedEnumMap $ mGammas model
     omegas <- getSharedEnumMap $ mOmegas model
     psis <- getSharedEnumMap $ mPsis model
     lambdas <- getSharedEnumMap $ mLambdas model
     phis <- getSharedEnumMap $ mPhis model
     ss <- getSharedEnumMap $ mSs model
     fs <- getSharedEnumMap $ mFs model
     ts <- getSharedEnumMap $ mTs model
     l <- likelihood model
     return $ STModelState { msData = mData model
                           , msNodeItems = mNodeItems model
                           , msGammas = gammas
                           , msOmegas = omegas
                           , msPsis = psis
                           , msLambdas = lambdas
                           , msPhis = phis
                           , msSs = ss
                           , msFs = fs
                           , msTs = ts
                           , msLogLikelihood = logFromLogFloat l
                           }

