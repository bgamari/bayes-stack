{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.LDA
  ( -- * Primitives
    LDAData(..)
  , Node(..), Item(..), Topic(..)
  , NodeItem, setupNodeItems
    -- * Model
  , LDAModel(..), ItemUnit
  , LDAModelState(..), getModelState
  , model, likelihood
  ) where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.Set (Set)
import qualified Data.Set as S

import Data.Traversable
import Data.Foldable
import Data.Monoid

import Control.Monad (liftM)
import Data.Random
import Data.Random.Sequence
import Data.Number.LogFloat

import BayesStack.Core
import BayesStack.Categorical
import BayesStack.DirMulti
import BayesStack.TupleEnum
import BayesStack.Models.Topic.Types

import GHC.Generics
import Data.Serialize

import Control.Monad (when)
import Control.Monad.IO.Class

data LDAData = LDAData { ldaAlphaTheta :: Double
                       , ldaAlphaPhi :: Double
                       , ldaNodes :: Set Node
                       , ldaItems :: Set Item
                       , ldaTopics :: Set Topic
                       , ldaNodeItems :: EnumMap NodeItem (Node, Item)
                       }
               deriving (Show, Eq, Generic)
instance Serialize LDAData

data LDAModel = LDAModel { mData :: LDAData
                         , mThetas :: SharedEnumMap Node (DirMulti Topic)
                         , mPhis :: SharedEnumMap Topic (DirMulti Item)
                         , mTs :: SharedEnumMap NodeItem Topic
                         } deriving (Generic)

data LDAModelState = LDAModelState { msData :: LDAData
                                   , msThetas :: EnumMap Node (DirMulti Topic)
                                   , msPhis :: EnumMap Topic (DirMulti Item)
                                   , msTs :: EnumMap NodeItem Topic
                                   , msLogLikelihood :: Double
                                   }
                     deriving (Show, Generic)
instance Serialize LDAModelState

data ItemUnit = ItemUnit { iuData :: LDAData
                         , iuNodeItem :: NodeItem
                         , iuN :: Node
                         , iuT :: Shared Topic
                         , iuX :: Item
                         , iuTheta :: Shared (DirMulti Topic)
                         , iuPhis :: SharedEnumMap Topic (DirMulti Item)
                         }

model :: LDAData -> ModelMonad (Seq ItemUnit, LDAModel)
model d =
  do let LDAData {ldaTopics=topics, ldaNodes=nodes, ldaItems=items, ldaNodeItems=nis} = d
     thetas <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (ldaAlphaTheta d) (S.toList topics)
     phis <- newSharedEnumMap (S.toList topics) $ \t ->
       return $ symDirMulti (ldaAlphaPhi d) (S.toList items)
     ts <- newSharedEnumMap (EM.keys nis) $ \ni ->
       liftRVar $ randomElementT $ SQ.fromList $ S.toList topics
  
     itemUnits <- forM (EM.toList ts) $ \(ni, t) ->
       do let (n,x) = nis EM.! ni
          let unit = ItemUnit { iuData = d 
                              , iuNodeItem = ni
                              , iuN = n
                              , iuT = t
                              , iuX = x
                              , iuTheta = thetas EM.! n
                              , iuPhis = phis
                              }
          getShared t >>= guSet unit
          return unit
     let model = LDAModel { mData = d
                          , mThetas = thetas
                          , mPhis = phis
                          , mTs = ts }
     return (SQ.fromList itemUnits, model)

likelihood :: LDAModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ ldaNodeItems $ mData model) $ \(ni, (n,i)) ->
       do t <- getShared $ mTs model EM.! ni 
          theta <- getShared $ mThetas model EM.! n
          phi <- getShared $ mPhis model EM.! t
          return $ Product $ logFloat (prob theta t)
                           * logFloat (prob phi i)
     return $ getProduct $ mconcat a

instance GibbsUpdateUnit ItemUnit where
  type GUValue ItemUnit = Topic
  guProb unit t =
    do phi <- getShared $ iuPhis unit EM.! t 
       theta <- getShared $ iuTheta unit
       let th = prob theta t
       let ph = prob phi (iuX unit) 
       return $ th * ph
  
  guDomain = return . S.toList . ldaTopics . iuData
  
  guUnset unit =
    do t <- getShared $ iuT unit 
       let x = iuX unit
           theta = iuTheta unit
           phi = iuPhis unit EM.! t
       theta `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
       return t
  
  guSet unit t =
    do iuT unit `setShared` t
       let x = iuX unit
           theta = iuTheta unit
           phi = iuPhis unit EM.! t
       theta `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x

getModelState :: LDAModel -> ModelMonad LDAModelState
getModelState model =
  do thetas <- getSharedEnumMap $ mThetas model
     phis <- getSharedEnumMap $ mPhis model
     ts <- getSharedEnumMap $ mTs model
     l <- likelihood model
     return $ LDAModelState { msData = mData model 
                            , msThetas = thetas
                            , msPhis = phis
                            , msTs = ts
                            , msLogLikelihood = logFromLogFloat l
                            }

