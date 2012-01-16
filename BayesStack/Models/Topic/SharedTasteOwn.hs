{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.SharedTasteOwn
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

import Control.Monad (liftM)
import Data.Random
import Data.Random.Distribution.Bernoulli
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

data STData = STData { stAlphaGammaOwn, stAlphaGammaShared :: Double
                     , stAlphaOmega :: Double
                     , stAlphaPsi :: Double
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

data ItemVars = ItemVars { ivS :: Bool
                         , ivF :: Node
                         , ivT :: Topic
                         }
                deriving (Show, Eq, Generic)
instance Serialize ItemVars

data STModel = STModel { mData :: STData
                       , mGammas :: SharedEnumMap Node (DirMulti Bool)
                       , mOmegas :: SharedEnumMap Node (DirMulti Topic)
                       , mPsis :: SharedEnumMap Node (DirMulti Node)
                       , mLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                       , mPhis :: SharedEnumMap Topic (DirMulti Item)
                       , mVars :: SharedEnumMap NodeItem ItemVars
                       }

data STModelState = STModelState { msData :: STData
                                 , msGammas :: EnumMap Node (DirMulti Bool)
                                 , msOmegas :: EnumMap Node (DirMulti Topic)
                                 , msPsis :: EnumMap Node (DirMulti Node)
                                 , msLambdas :: EnumMap Friendship (DirMulti Topic)
                                 , msPhis :: EnumMap Topic (DirMulti Item)
                                 , msVars :: EnumMap NodeItem ItemVars
                                 , msLogLikelihood :: Double
                                 }
                    deriving (Show, Generic)
instance Serialize STModelState

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
                         s <- bernoulli (0.5::Double)
                         return $ EM.singleton ni $ ItemVars s f t
  in liftM mconcat $ forM (ES.toList unset) randomInit

randomInitialize :: STData -> RVar ModelInit
randomInitialize = (flip randomInitialize') EM.empty

data ItemUnit = ItemUnit { iuModel :: STModel
                         , iuNodeItem :: NodeItem
                         , iuFriends :: Set Node
                         , iuN :: Node
                         , iuVars :: Shared ItemVars
                         , iuX :: Item
                         , iuGamma :: Shared (DirMulti Bool)
                         , iuOmega :: Shared (DirMulti Topic)
                         , iuPsi :: Shared (DirMulti Node)
                         , iuLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                         , iuPhis :: SharedEnumMap Topic (DirMulti Item)
                         }

model :: STData -> ModelInit -> ModelMonad (Seq ItemUnit, STModel)
model d init =
  do let STData {stTopics=topics, stNodes=nodes, stItems=items, stNodeItems=nis} = d
         STData {stFriendships=friendships} = d
         friends :: EnumMap Node (Set Node)
         friends = foldMap (\n->EM.singleton n $ S.fromList $ getFriends (S.toList friendships) n) nodes
     gammas <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ dirMulti [(True, stAlphaGammaShared d), (False, stAlphaGammaOwn d)]
     omegas <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaOmega d) (S.toList topics)
     psis <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaPsi d) (S.toList nodes)
     lambdas <- newSharedEnumMap (S.toList friendships) $ \n ->
       return $ symDirMulti (stAlphaLambda d) (S.toList topics)
     phis <- newSharedEnumMap (S.toList topics) $ \t ->
       return $ symDirMulti (stAlphaPhi d) (S.toList items)

     ivs <- newSharedEnumMap (EM.keys nis) $ \ni -> return $ init EM.! ni
  
     let model = STModel { mData = d
                         , mGammas = gammas
                         , mOmegas = omegas
                         , mPsis = psis
                         , mLambdas = lambdas
                         , mPhis = phis
                         , mVars = ivs
                         }

     itemUnits <- forM (EM.toList ivs) $ \(ni,iv) ->
       do let (n,x) = nis EM.! ni
              unit = ItemUnit { iuModel = model
                              , iuNodeItem = ni
                              , iuFriends = friends EM.! n
                              , iuN = n
                              , iuVars = iv
                              , iuX = x
                              , iuGamma = gammas EM.! n
                              , iuOmega = omegas EM.! n
                              , iuPsi = psis EM.! n
                              , iuLambdas = EM.filterWithKey (\k _->isFriend n k) lambdas
                              , iuPhis = phis
                              }
          getShared iv >>= guSet unit
          return unit
     return (SQ.fromList itemUnits, model)

likelihood :: STModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ stNodeItems $ mData model) $ \(ni, (n,x)) ->
       do ItemVars s f t <- getShared $ mVars model EM.! ni 
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
  type GUValue ItemUnit = ItemVars
  guProb unit (ItemVars s f t) =
    do gamma <- getShared $ iuGamma unit
       omega <- getShared $ iuOmega unit
       psi <- getShared $ iuPsi unit
       phi <- getShared $ iuPhis unit EM.! t 
       lambda <- getShared $ iuLambdas unit EM.! Friendship (iuN unit, f)
       if s then return $ prob gamma s * prob psi f * prob lambda t * prob phi (iuX unit) 
            else return $ prob gamma s * prob omega t * prob phi (iuX unit)
  
  guDomain unit = return $ (do t <- S.toList $ stTopics $ mData $ iuModel unit
                               f <- S.toList $ iuFriends unit
                               return $ ItemVars True f t)
                        ++ (do t <- S.toList $ stTopics $ mData $ iuModel unit
                               let f = head $ S.toList $ iuFriends unit
                               return $ ItemVars False f t)
  
  guUnset unit =
    do ItemVars s f t <- getShared $ iuVars unit 
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
       return $ ItemVars s f t
  
  guSet unit iv@(ItemVars s f t) =
    do iuVars unit `setShared` iv
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
     vars <- getSharedEnumMap $ mVars model
     l <- likelihood model
     return $ STModelState { msData = mData model
                           , msGammas = gammas
                           , msOmegas = omegas
                           , msPsis = psis
                           , msLambdas = lambdas
                           , msPhis = phis
                           , msVars = vars
                           , msLogLikelihood = logFromLogFloat l
                           }

