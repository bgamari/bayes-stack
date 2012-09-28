{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric, TupleSections #-}

module BayesStack.Models.Topic.CitationInfluence
  ( -- * Primitives
    NetData(..)
  , MState(..)
  , CitedUpdateUnit
  , CitingUpdateUnit
  , ItemSource(..)
  , CitedNode(..), CitedNodeItem(..)
  , CitingNode(..), CitingNodeItem(..)
  , Item(..), Topic(..), Arc(..)
  , setupNodeItems
    -- * Initialization
  , ModelInit
  , randomInitialize
  , model
  , updateUnits
    -- * Diagnostics
  , modelLikelihood
  ) where

import           Prelude hiding (mapM_)

import           Data.Set (Set)
import qualified Data.Set as S

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Data.Foldable hiding (product)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Trans.State.Strict

import           Data.Random
import           Data.Random.Lift (lift)
import           Data.Random.Distribution.Categorical (categorical)
import           Data.Number.LogFloat hiding (realToFrac)

import           BayesStack.Core.Types
import           BayesStack.Core.Gibbs
import           BayesStack.DirMulti
import           BayesStack.TupleEnum ()
import           BayesStack.Models.Topic.Types

import           GHC.Generics
import           Data.Serialize (Serialize)
import           Control.DeepSeq                 

data ItemSource = Shared | Own deriving (Show, Eq, Enum, Ord, Generic)
instance Serialize ItemSource
instance NFData ItemSource         
         
newtype CitingNode = CitingNode Int deriving (Show, Eq, Enum, Ord, Generic, NFData)
newtype CitedNode = CitedNode Int deriving (Show, Eq, Enum, Ord, Generic, NFData)
instance Serialize CitingNode
instance Serialize CitedNode

newtype CitingNodeItem = CitingNI Int deriving (Show, Eq, Enum, Ord, Generic)
newtype CitedNodeItem = CitedNI Int deriving (Show, Eq, Enum, Ord, Generic)
instance Serialize CitingNodeItem
instance Serialize CitedNodeItem

-- ^ A directed edge         
newtype Arc = Arc (CitingNode, CitedNode)
            deriving (Show, Eq, Ord, Generic)
instance Serialize Arc

-- ^ The citing node of an arc
citingNode :: Arc -> CitingNode
citingNode (Arc (a,_)) = a           

-- ^ The cited node of an arc
citedNode :: Arc -> CitedNode
citedNode (Arc (_,b)) = b

data NetData = NetData { dAlphaPsi           :: Double
                       , dAlphaLambda        :: Double
                       , dAlphaPhi           :: Double
                       , dAlphaOmega         :: Double
                       , dAlphaGammaShared   :: Double
                       , dAlphaGammaOwn      :: Double
                       , dArcs               :: Set Arc
                       , dItems              :: Set Item
                       , dTopics             :: Set Topic
                       , dCitedNodeItems     :: Map CitedNodeItem (CitedNode, Item)
                       , dCitingNodeItems    :: Map CitingNodeItem (CitingNode, Item)
                       }
              deriving (Show, Eq, Generic)
instance Serialize NetData
         
dCitingNodes :: NetData -> Set CitingNode
dCitingNodes = S.fromList . map fst . M.elems . dCitingNodeItems

dCitedNodes :: NetData -> Set CitedNode
dCitedNodes = S.fromList . map fst . M.elems . dCitedNodeItems
         
getCitingNodes :: NetData -> CitedNode -> Set CitingNode
getCitingNodes d n = S.map citingNode $ S.filter (\(Arc (_,cited))->cited==n) $ dArcs d

getCitedNodes :: NetData -> CitingNode -> Set CitedNode
getCitedNodes d n = S.map citedNode $ S.filter (\(Arc (citing,_))->citing==n) $ dArcs d

type CitedModelInit = Map CitedNodeItem (Setting CitedUpdateUnit)
type CitingModelInit = Map CitingNodeItem (Setting CitingUpdateUnit)
data ModelInit = ModelInit CitedModelInit CitingModelInit
               deriving (Show)

randomInitializeCited :: NetData -> CitedModelInit -> RVar CitedModelInit
randomInitializeCited d init = execStateT doInit init
    where doInit = let unset = M.keysSet (dCitedNodeItems d) `S.difference` M.keysSet init
                   in mapM_ (randomInitCitedUU d) (S.toList unset)

modify' :: Monad m => (a -> a) -> StateT a m ()
modify' f = do x <- get
               put $! f x

randomInitCitedUU :: NetData -> CitedNodeItem -> StateT CitedModelInit RVar ()
randomInitCitedUU d ni = do
    t' <- lift $ randomElement $ toList $ dTopics d
    modify' $ M.insert ni t'

randomInitializeCiting :: NetData -> CitingModelInit -> RVar CitingModelInit
randomInitializeCiting d init = execStateT doInit init
    where doInit :: StateT CitingModelInit RVar ()
          doInit = let unset = M.keysSet (dCitingNodeItems d) `S.difference` M.keysSet init
                   in mapM_ (randomInitCitingUU d) (S.toList unset)
   
randomInitCitingUU :: NetData -> CitingNodeItem -> StateT CitingModelInit RVar ()
randomInitCitingUU d ni =
    let (n,_) = dCitingNodeItems d M.! ni
    in case getCitedNodes d n of
           a | S.null a -> do
               t <- lift $ randomElement $ toList $ dTopics d
               modify' $ M.insert ni (Own, CitedNode 0, t)

           citedNodes -> do
               s <- lift $ randomElement [Shared, Own]
               c <- lift $ randomElement $ toList $ getCitedNodes d n
               t <- lift $ randomElement $ toList $ dTopics d
               modify' $ M.insert ni (s,c,t)

randomInitialize :: NetData -> RVar ModelInit
randomInitialize d =
    ModelInit <$> randomInitializeCited d M.empty <*> randomInitializeCiting d M.empty
                
model :: NetData -> ModelInit -> MState
model d (ModelInit citedInit citingInit) =
    let s = MState { -- Citing model
                     stPsis = let dist = symDirMulti (dAlphaPsi d) (toList $ dCitedNodes d)
                              in foldMap (\n->M.singleton n dist) $ dCitingNodes d
                   , stPhis = let dist = symDirMulti (dAlphaPhi d) (toList $ dItems d)
                              in foldMap (\t->M.singleton t dist) $ dTopics d
                   , stGammas = let dist = multinom [ (Shared, dAlphaGammaShared d)
                                                    , (Own, dAlphaGammaOwn d) ]
                                in foldMap (\t->M.singleton t dist) $ dCitingNodes d
                   , stOmegas = let dist = symDirMulti (dAlphaOmega d) (toList $ dTopics d)
                                in foldMap (\t->M.singleton t dist) $ dCitingNodes d
                   , stC = M.empty
                   , stS = M.empty
                   , stT = M.empty

                   -- Cited model
                   , stLambdas = let dist = symDirMulti (dAlphaLambda d) (toList $ dTopics d)
                                 in foldMap (\t->M.singleton t dist) $ dCitedNodes d
                   , stT' = M.empty
                   }

        initCitingUU :: CitingUpdateUnit -> State MState ()
        initCitingUU uu = do
            let err = error $ "CitationInference: Initial value for "++show uu++" not given\n"
                            ++show citingInit++"\n\n"
                            ++show (M.findMax citingInit, M.findMin citingInit)++"\n\n"
                            ++show (fst (M.findMax citingInit) == uuNI uu)++"\n\n"
                            ++show (M.lookup (uuNI uu) citingInit)
                            ++show (M.findWithDefault (error "hi") (uuNI uu) citingInit)
                s = maybe err id $ M.lookup (uuNI uu) citingInit
            modify' $ setCitingUU uu (Just s)

        initCitedUU :: CitedUpdateUnit -> State MState ()
        initCitedUU uu = do
            let err = error $ "CitationInference: Initial value for "++show uu++" not given"
                s = maybe err id $ M.lookup (uuNI' uu) citedInit
            modify' $ setCitedUU uu (Just s)

    in execState (do mapM_ initCitingUU $ citingUpdateUnits d
                     mapM_ initCitedUU $ citedUpdateUnits d
                 ) s

updateUnits :: NetData -> [WrappedUpdateUnit MState]
updateUnits d = map WrappedUU (citedUpdateUnits d)
             ++ map WrappedUU (citingUpdateUnits d)

data MState = MState { -- Citing model state
                       stGammas   :: Map CitingNode (Multinom ItemSource)
                     , stOmegas   :: Map CitingNode (Multinom Topic)
                     , stPsis     :: Map CitingNode (Multinom CitedNode)
                     , stPhis     :: Map Topic (Multinom Item)

                     , stC        :: Map CitingNodeItem CitedNode
                     , stS        :: !(Map CitingNodeItem ItemSource)
                     , stT        :: Map CitingNodeItem Topic

                     -- Cited model state
                     , stLambdas  :: Map CitedNode (Multinom Topic)

                     , stT'       :: Map CitedNodeItem Topic
                     }
            deriving (Show, Generic)
instance Serialize MState

modelLikelihood :: MState -> Probability
modelLikelihood model =
    product $ map likelihood (M.elems $ stGammas model)
           ++ map likelihood (M.elems $ stPhis model)
           ++ map likelihood (M.elems $ stLambdas model)
           ++ map likelihood (M.elems $ stOmegas model)
           ++ map likelihood (M.elems $ stPsis model)

-- Cited update unit (LDA-like)
data CitedUpdateUnit = CitedUpdateUnit { uuNI' :: CitedNodeItem
                                       , uuN'  :: CitedNode
                                       , uuX' :: Item
                                       }
                     deriving (Show, Generic)
instance Serialize CitedUpdateUnit

instance UpdateUnit CitedUpdateUnit where
    type ModelState CitedUpdateUnit = MState
    type Setting CitedUpdateUnit = Topic
    fetchSetting uu ms = stT' ms M.! uuNI' uu
    evolveSetting ms uu = categorical $ citedFullCond (setCitedUU uu Nothing ms) uu
    updateSetting uu _ s' = setCitedUU uu (Just s') . setCitedUU uu Nothing

citedProb :: MState -> CitedUpdateUnit -> Setting CitedUpdateUnit -> Double
citedProb st (CitedUpdateUnit {uuN'=n', uuX'=x'}) t =
    let lambda = stLambdas st M.! n'
        phi = stPhis st M.! t
    in realToFrac $ sampleProb lambda t * sampleProb phi x'

citedUpdateUnits :: NetData -> [CitedUpdateUnit]
citedUpdateUnits d =
    map (\(ni',(n',x'))->CitedUpdateUnit { uuNI'      = ni'
                                         , uuN'       = n'
                                         , uuX'       = x'
                                         }
        ) $ M.assocs $ dCitedNodeItems d
              
setCitedUU :: CitedUpdateUnit -> Maybe Topic -> MState -> MState
setCitedUU uu@(CitedUpdateUnit {uuN'=n', uuNI'=ni', uuX'=x'}) setting ms =
    let t' = maybe (fetchSetting uu ms) id setting
        set = maybe Unset (const Set) setting
    in ms { stLambdas = M.adjust (setMultinom set t') n' (stLambdas ms)
          , stPhis = M.adjust (setMultinom set x') t' (stPhis ms)
          , stT' = case setting of Just _  -> M.insert ni' t' $ stT' ms
                                   Nothing -> stT' ms
          }

citedFullCond ::MState -> CitedUpdateUnit -> [(Double, Topic)]
citedFullCond ms uu = do
    t <- M.keys $ stPhis ms
    return (citedProb ms uu t, t)


-- Citing Update unit (Shared Taste-like)
data CitingUpdateUnit = CitingUpdateUnit { uuNI    :: CitingNodeItem
                                         , uuN     :: CitingNode
                                         , uuX     :: Item
                                         , uuCites :: Set CitedNode
                                         }
                      deriving (Show, Generic)
instance Serialize CitingUpdateUnit

instance UpdateUnit CitingUpdateUnit where
    type ModelState CitingUpdateUnit = MState
    type Setting CitingUpdateUnit = (ItemSource, CitedNode, Topic)
    fetchSetting uu ms = ( stS ms M.! uuNI uu
                         , stC ms M.! uuNI uu
                         , stT ms M.! uuNI uu
                         )
    evolveSetting ms uu = categorical $ citingFullCond (setCitingUU uu Nothing ms) uu
    updateSetting uu _ s' = setCitingUU uu (Just s') . setCitingUU uu Nothing

citingUpdateUnits :: NetData -> [CitingUpdateUnit]
citingUpdateUnits d =
    map (\(ni,(n,x))->CitingUpdateUnit { uuNI      = ni
                                       , uuN       = n
                                       , uuX       = x
                                       , uuCites = getCitedNodes d n
                                       }
        ) $ M.assocs $ dCitingNodeItems d
        
citingProb :: MState -> CitingUpdateUnit -> Setting CitingUpdateUnit -> Double
citingProb st (CitingUpdateUnit {uuN=n, uuX=x}) (s,c,t) =
    let gamma = stGammas st M.! n
        omega = stOmegas st M.! n
        phi = stPhis st M.! t
        psi = stPsis st M.! n
        lambda = stLambdas st M.! c
    in case s of 
        Shared ->   sampleProb gamma s
                  * sampleProb psi c
                  * sampleProb lambda t
                  * sampleProb phi x
        Own ->   sampleProb gamma s
               * sampleProb omega t
               * sampleProb phi x

citingFullCond :: MState -> CitingUpdateUnit -> [(Double, Setting CitingUpdateUnit)]
citingFullCond ms uu = map (\s->(citingProb ms uu s, s)) $ citingDomain ms uu
            
citingDomain :: MState -> CitingUpdateUnit -> [Setting CitingUpdateUnit]
citingDomain ms uu = do
    s <- [Own, Shared]
    t <- M.keys $ stPhis ms
    case s of
        Shared -> do c <- S.toList $ uuCites uu
                     return (Shared, c, t)
        Own    -> do return (Own, error "No C for own item", t)

setCitingUU :: CitingUpdateUnit -> Maybe (Setting CitingUpdateUnit) -> MState -> MState
setCitingUU uu@(CitingUpdateUnit {uuNI=ni, uuN=n, uuX=x}) setting ms =
    let set = maybe Unset (const Set) setting
        (s,c,t) = maybe (fetchSetting uu ms) id setting
        ms' = case s of
            Shared -> ms { stPsis = M.adjust (setMultinom set c) n $ stPsis ms
                         , stLambdas = M.adjust (setMultinom set t) c $ stLambdas ms
                         }
            Own    -> ms { stOmegas = M.adjust (setMultinom set t) n $ stOmegas ms }
    in ms' { stPhis = M.adjust (setMultinom set x) t $ stPhis ms
           , stGammas = M.adjust (setMultinom set s) n $ stGammas ms
           , stT = case setting of Just _  -> M.insert ni t $ stT ms
                                   Nothing -> stT ms
           , stC = case setting of Just _  -> M.insert ni c $ stC ms
                                   Nothing -> stC ms
           , stS = case setting of Just _  -> M.insert ni s $ stS ms
                                   Nothing -> stS ms
           }

