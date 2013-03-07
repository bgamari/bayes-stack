{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.LDA
  ( -- * Primitives
    NetData(..)
  , MState(..)
  , LDAUpdateUnit
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

import BayesStack.Core.Types
import BayesStack.Core.Gibbs
import BayesStack.DirMulti
import BayesStack.TupleEnum ()
import BayesStack.Models.Topic.Types

import GHC.Generics
import Data.Binary

data NetData = NetData { dAlphaTheta :: !Double
                       , dAlphaPhi :: !Double
                       , dNodes :: !(Set Node)
                       , dItems :: !(Set Item)
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
updateUnits' =
    map (\(ni,(n,x))->LDAUpdateUnit {uuNI=ni, uuN=n, uuX=x}) . M.assocs . dNodeItems

model :: NetData -> ModelInit -> MState
model d init =
    let uus = updateUnits' d
        s = MState { stThetas = foldMap (\n->M.singleton n (symDirMulti (dAlphaTheta d) (toList $ dTopics d)))
                                $ dNodes d
                   , stPhis = foldMap (\t->M.singleton t (symDirMulti (dAlphaPhi d) (toList $ dItems d)))
                              $ dTopics d
                   , stT = M.empty
                   }
    in execState (mapM (\uu->modify $ setUU uu (Just $ M.findWithDefault (Topic 0) (uuNI uu) init)) uus) s

data MState = MState { stThetas :: !(Map Node (Multinom Int Topic))
                     , stPhis   :: !(Map Topic (Multinom Int Item))
                     , stT      :: !(Map NodeItem Topic)
                     }
            deriving (Show, Generic)
instance Binary MState

data LDAUpdateUnit = LDAUpdateUnit { uuNI :: NodeItem
                                   , uuN  :: Node
                                   , uuX  :: Item
                                   }
                   deriving (Show, Generic)
instance Binary LDAUpdateUnit

setUU :: LDAUpdateUnit -> Maybe Topic -> MState -> MState
setUU uu@(LDAUpdateUnit {uuN=n, uuNI=ni, uuX=x}) setting ms =
    let t = maybe (fetchSetting uu ms) id setting
        set = maybe Unset (const Set) setting
    in ms { stPhis = M.adjust (setMultinom set x) t (stPhis ms)
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
    product $ map likelihood (M.elems $ stThetas model)
           ++ map likelihood (M.elems $ stPhis model)

-- | Re-estimate phi hyperparameter
reestimatePhis :: MState -> MState
reestimatePhis ms = ms { stPhis = reestimateSymPriors $ stPhis ms }

-- | Re-estimate theta hyperparameter
reestimateThetas :: MState -> MState
reestimateThetas ms = ms { stThetas = reestimateSymPriors $ stThetas ms }

reestimate :: MState -> MState
reestimate = reestimatePhis . reestimateThetas
