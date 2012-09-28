{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.LDA
  ( -- * Primitives
    LDAData(..)
  , LDAState(..)
  , LDAUpdateUnit
  , Node(..), Item(..), Topic(..)
  , NodeItem(..), setupNodeItems
    -- * Initialization
  , ModelInit
  , randomInitialize
  , model, updateUnits
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
import Data.Serialize

data LDAData = LDAData { ldaAlphaTheta :: Double
                       , ldaAlphaPhi :: Double
                       , ldaNodes :: Set Node
                       , ldaItems :: Set Item
                       , ldaTopics :: Set Topic
                       , ldaNodeItems :: Map NodeItem (Node, Item)
                       }
               deriving (Show, Eq, Generic)
instance Serialize LDAData

type ModelInit = Map NodeItem Topic

randomInitialize' :: LDAData -> ModelInit -> RVar ModelInit
randomInitialize' d init = 
  let unset = M.keysSet (ldaNodeItems d) `S.difference` M.keysSet init
      topics = S.toList $ ldaTopics d
      randomInit :: NodeItem -> RVar ModelInit
      randomInit ni = liftM (M.singleton ni) $ randomElement topics
  in liftM mconcat $ forM (S.toList unset) randomInit

randomInitialize :: LDAData -> RVar ModelInit
randomInitialize = (flip randomInitialize') M.empty
                
updateUnits :: LDAData -> [WrappedUpdateUnit LDAState]
updateUnits = map WrappedUU . updateUnits'            

updateUnits' :: LDAData -> [LDAUpdateUnit]
updateUnits' =
    map (\(ni,(n,x))->LDAUpdateUnit {uuNI=ni, uuN=n, uuX=x}) . M.assocs . ldaNodeItems 
              
model :: LDAData -> ModelInit -> LDAState
model d init =
    let uus = updateUnits' d
        s = LDAState { stThetas = foldMap (\n->M.singleton n (symDirMulti (ldaAlphaTheta d) (toList $ ldaTopics d)))
                                  $ ldaNodes d
                     , stPhis = foldMap (\t->M.singleton t (symDirMulti (ldaAlphaPhi d) (toList $ ldaItems d)))
                                $ ldaTopics d
                     , stT = M.empty
                     }
    in execState (mapM (\uu->modify $ setUU uu (Just $ M.findWithDefault (Topic 0) (uuNI uu) init)) uus) s

data LDAState = LDAState { stThetas :: Map Node (Multinom Topic)
                         , stPhis   :: Map Topic (Multinom Item)
                         , stT      :: Map NodeItem Topic
                         }
              deriving (Show, Generic)
instance Serialize LDAState

data LDAUpdateUnit = LDAUpdateUnit { uuNI :: NodeItem
                                   , uuN  :: Node
                                   , uuX  :: Item
                                   }
                   deriving (Show, Generic)
instance Serialize LDAUpdateUnit

setUU :: LDAUpdateUnit -> Maybe Topic -> LDAState -> LDAState
setUU uu@(LDAUpdateUnit {uuN=n, uuNI=ni, uuX=x}) setting ms =
    let t = maybe (fetchSetting uu ms) id setting
        set = maybe Unset (const Set) setting
    in ms { stPhis = M.adjust (setMultinom set x) t (stPhis ms)
          , stThetas = M.adjust (setMultinom set t) n (stThetas ms)
          , stT = case setting of Just _  -> M.insert ni t $ stT ms
                                  Nothing -> stT ms
          }

instance UpdateUnit LDAUpdateUnit where
    type ModelState LDAUpdateUnit = LDAState
    type Setting LDAUpdateUnit = Topic
    fetchSetting (LDAUpdateUnit {uuNI=ni}) ms = stT ms M.! ni
    evolveSetting ms uu = categorical $ ldaFullCond (setUU uu Nothing ms) uu
    updateSetting uu _ s' = setUU uu (Just s') . setUU uu Nothing
        
uuProb :: LDAState -> LDAUpdateUnit -> Topic -> Double
uuProb state (LDAUpdateUnit {uuN=n, uuX=x}) t =
    let theta = stThetas state M.! n
        phi = stPhis state M.! t
    in realToFrac $ sampleProb theta t * sampleProb phi x

ldaFullCond :: LDAState -> LDAUpdateUnit -> [(Double, Topic)]
ldaFullCond ms uu = do
    t <- M.keys $ stPhis ms
    return (uuProb ms uu t, t)

modelLikelihood :: LDAState -> Probability
modelLikelihood model =
  product $ map likelihood (M.elems $ stThetas model)
         ++ map likelihood (M.elems $ stPhis model)

