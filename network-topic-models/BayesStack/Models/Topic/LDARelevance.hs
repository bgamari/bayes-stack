{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric, FlexibleInstances, RecordWildCards #-}

module BayesStack.Models.Topic.LDARelevance
  ( -- * Primitives
    NetData(..)
  , HyperParams(..)
  , MState(..)
  , LDAUpdateUnit
  , ItemWeight
  , Node(..), Item(..), Topic(..)
  , NodeItem(..), setupNodeItems
    -- * Initialization
  , ModelInit
  , randomInitialize
  , model, updateUnits
    -- * Hyperparameter estimation
  , reestimate, reestimatePhis, reestimateThetas
    -- * Diagnostics
  , modelLikelihood
  ) where

import Prelude hiding (mapM)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Traversable
import Data.Foldable hiding (product)
import Data.Monoid

import Control.Monad (liftM)
import Control.Monad.Trans.State
import Data.Random
import Data.Random.Distribution.Categorical (categorical)

import BayesStack.Types
import BayesStack.Gibbs
import BayesStack.DirMulti
import BayesStack.TupleEnum ()
import BayesStack.Models.Topic.Types

import GHC.Generics
import Data.Binary as B
import Data.Fixed

type ItemWeight = Micro

data HyperParams = HyperParams
                   { alphaTheta       :: Double
                   , alphaPhi         :: Double
                   }
                 deriving (Show, Eq, Generic)
instance Binary HyperParams     

data NetData = NetData { dHypers :: !HyperParams
                       , dNodes :: !(Set Node)
                       , dItems :: !(Map Item ItemWeight)
                       , dTopics :: !(Set Topic)
                       , dNodeItems :: !(Map NodeItem (Node, Item))
                       }
               deriving (Show, Eq, Generic)
instance Binary NetData

type ModelInit = Map NodeItem Topic

randomInitialize' :: NetData -> ModelInit -> RVar ModelInit
randomInitialize' d init =
  let unset = M.keysSet (dNodeItems d) `S.difference` M.keysSet init
      topics = S.toList $ dTopics d
      randomInit :: NodeItem -> RVar ModelInit
      randomInit ni = liftM (M.singleton ni) $ randomElement topics
  in liftM mconcat $ forM (S.toList unset) randomInit

randomInitialize :: NetData -> RVar ModelInit
randomInitialize = (flip randomInitialize') M.empty

updateUnits :: NetData -> [WrappedUpdateUnit MState]
updateUnits = map WrappedUU . updateUnits'

updateUnits' :: NetData -> [LDAUpdateUnit]
updateUnits' nd =
    map (\(ni,(n,x))->LDAUpdateUnit { uuNI=ni, uuN=n, uuX=x
                                    , uuW=dItems nd M.! x
                                    })
    $ M.assocs $ dNodeItems nd

model :: NetData -> ModelInit -> MState
model d init =
    let uus = updateUnits' d
        s = MState { stThetas = foldMap (\n->M.singleton n (symDirMulti alphaTheta (toList $ dTopics d)))
                                $ dNodes d
                   , stPhis = foldMap (\t->M.singleton t (symDirMulti alphaPhi (M.keys $ dItems d)))
                              $ dTopics d
                   , stT = M.empty
                   }
        HyperParams {..} = dHypers d
    in execState (mapM (\uu->modify $ setUU uu (Just $ M.findWithDefault (Topic 0) (uuNI uu) init)) uus) s

data MState = MState { stThetas :: !(Map Node (Multinom Int Topic))
                     , stPhis   :: !(Map Topic (Multinom ItemWeight Item))
                     , stT      :: !(Map NodeItem Topic)
                     }
            deriving (Show, Generic)
instance Binary MState

data LDAUpdateUnit = LDAUpdateUnit { uuNI :: NodeItem
                                   , uuN  :: Node
                                   , uuX  :: Item
                                   , uuW  :: ItemWeight
                                   }
                   deriving (Show, Generic)
instance Binary LDAUpdateUnit

instance Binary (Fixed E6) where
    get = do a <- B.get :: Get Int
             return $ fromIntegral a / 1000000
    put = (B.put :: Int -> Put) . round . (*1000000)

setUU :: LDAUpdateUnit -> Maybe Topic -> MState -> MState
setUU uu@(LDAUpdateUnit {uuN=n, uuNI=ni, uuX=x, uuW=w}) setting ms =
    let t = maybe (fetchSetting uu ms) id setting
        set = maybe Unset (const Set) setting
        setPhi = case setting of
                     Just _  -> addMultinom w x
                     Nothing -> subMultinom w x
    in ms { stPhis = M.adjust setPhi t (stPhis ms)
          , stThetas = M.adjust (setMultinom set t) n (stThetas ms)
          , stT = case setting of Just _  -> M.insert ni t $ stT ms
                                  Nothing -> stT ms
          }

instance UpdateUnit LDAUpdateUnit where
    type ModelState LDAUpdateUnit = MState
    type Setting LDAUpdateUnit = Topic
    fetchSetting (LDAUpdateUnit {uuNI=ni}) ms = stT ms M.! ni
    evolveSetting ms uu = categorical $ ldaFullCond (setUU uu Nothing ms) uu
    updateSetting uu _ s' = setUU uu (Just s') . setUU uu Nothing

uuProb :: MState -> LDAUpdateUnit -> Topic -> Double
uuProb state (LDAUpdateUnit {uuN=n, uuX=x}) t =
    let theta = stThetas state M.! n
        phi = stPhis state M.! t
    in realToFrac $ sampleProb theta t * sampleProb phi x

ldaFullCond :: MState -> LDAUpdateUnit -> [(Double, Topic)]
ldaFullCond ms uu = do
    t <- uuDomain ms uu
    return (uuProb ms uu t, t)

uuDomain :: MState -> LDAUpdateUnit -> [Topic]
uuDomain ms uu = M.keys $ stPhis ms

modelLikelihood :: MState -> Probability
modelLikelihood model =
    product (map likelihood $ M.elems $ stThetas model)
  * product (map likelihood $ M.elems $ stPhis model)

-- | Re-estimate phi hyperparameter
reestimatePhis :: MState -> MState
reestimatePhis ms = ms { stPhis = reestimateSymPriors $ stPhis ms }

-- | Re-estimate theta hyperparameter
reestimateThetas :: MState -> MState
reestimateThetas ms = ms { stThetas = reestimateSymPriors $ stThetas ms }

reestimate :: MState -> MState
reestimate = reestimatePhis . reestimateThetas
