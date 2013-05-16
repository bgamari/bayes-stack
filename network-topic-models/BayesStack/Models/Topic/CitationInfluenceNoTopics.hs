{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric, TupleSections #-}

module BayesStack.Models.Topic.CitationInfluenceNoTopics
  ( -- * Primitives
    NetData(..)
  , MState(..)
  , CitingUpdateUnit
  , ItemSource(..)
  , CitedNode(..), CitedNodeItem(..)
  , CitingNode(..), CitingNodeItem(..)
  , Citing(..), Cited(..)
  , Item(..), Topic(..), NodeItem(..), Node(..)
  , Arc(..), citedNode, citingNode
  , dCitedNodes, dCitingNodes, getCitingNodes, getCitedNodes
  , setupNodeItems
    -- * Initialization
  , verifyNetData, cleanNetData
  , ModelInit
  , randomInitialize
  , model
  , updateUnits
    -- * Diagnostics
  , modelLikelihood
  ) where

import qualified Data.Vector as V
import Statistics.Sample (mean)

import           Prelude hiding (mapM_, sum)

import           Data.Set (Set)
import qualified Data.Set as S

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Data.Foldable hiding (product)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (when)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.Strict

import           Data.Random
import           Data.Random.Lift (lift)
import           Data.Random.Distribution.Categorical (categorical)
import           Numeric.Log hiding (sum)

import           BayesStack.Core.Types
import           BayesStack.Core.Gibbs
import           BayesStack.DirMulti
import           BayesStack.TupleEnum ()
import           BayesStack.Models.Topic.Types

import           GHC.Generics
import           Data.Binary (Binary)
import           Control.DeepSeq

data ItemSource = Shared | Own deriving (Show, Eq, Enum, Ord, Generic)
instance Binary ItemSource
instance NFData ItemSource

newtype Citing a = Citing a deriving (Show, Eq, Enum, Ord, Generic, NFData)
newtype Cited a = Cited a deriving (Show, Eq, Enum, Ord, Generic, NFData)
instance Binary a => Binary (Citing a)
instance Binary a => Binary (Cited a)

type CitingNode = Citing Node
type CitedNode = Cited Node
type CitingNodeItem = Citing NodeItem
type CitedNodeItem = Cited NodeItem

-- ^ A directed edge
newtype Arc = Arc (CitingNode, CitedNode)
            deriving (Show, Eq, Ord, Generic)
instance Binary Arc

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
                       , dAlphaBetaFG        :: Double
                       , dAlphaBetaBG        :: Double
                       , dArcs               :: Set Arc
                       , dItems              :: Map Item Double
                       , dNodeItems          :: Map NodeItem (Node, Item)
                       }
              deriving (Show, Eq, Generic)
instance Binary NetData

dCitingNodeItems :: NetData -> Map CitingNodeItem (CitingNode, Item)
dCitingNodeItems nd =
    M.mapKeys Citing
    $ M.map (\(n,i)->(Citing n, i))
    $ M.filter (\(n,i)->Citing n `S.member` dCitingNodes nd)
    $ dNodeItems nd

dCitingNodes :: NetData -> Set CitingNode
dCitingNodes = S.map citingNode . dArcs

dCitedNodes :: NetData -> Set CitedNode
dCitedNodes = S.map citedNode . dArcs

getCitingNodes :: NetData -> CitedNode -> Set CitingNode
getCitingNodes d n = S.map citingNode $ S.filter (\(Arc (_,cited))->cited==n) $ dArcs d

getCitedNodes :: NetData -> CitingNode -> Set CitedNode
getCitedNodes d n = S.map citedNode $ S.filter (\(Arc (citing,_))->citing==n) $ dArcs d

itemsOfCitingNode :: NetData -> CitingNode -> [Item]
itemsOfCitingNode d (Citing u) =
    map snd $ M.elems $ M.filter (\(n,_)->n==u) $ dNodeItems d

connectedNodes :: Set Arc -> Set Node
connectedNodes arcs =
    S.map ((\(Cited n)->n) . citedNode) arcs `S.union` S.map ((\(Citing n)->n) . citingNode) arcs

cleanNetData :: NetData -> NetData
cleanNetData d =
    let nodesWithItems = S.fromList $ map fst $ M.elems $ dNodeItems d
        nodesWithArcs = connectedNodes $ dArcs d
        keptNodes = nodesWithItems `S.intersection` nodesWithArcs
        keepArc (Arc (Citing citing, Cited cited)) =
            citing `S.member` keptNodes && cited `S.member` keptNodes
    in d { dArcs = S.filter keepArc $ dArcs d
         , dNodeItems = M.filter (\(n,i)->n `S.member` keptNodes) $ dNodeItems d
         }

verifyNetData :: (Node -> String) -> NetData -> [String]
verifyNetData showNode d = execWriter $ do
    let nodesWithItems = S.fromList $ map fst $ M.elems $ dNodeItems d
    forM_ (dArcs d) $ \(Arc (Citing citing, Cited cited))->do
        when (cited `S.notMember` nodesWithItems)
            $ tell [showNode cited++" has arc yet has no items"]
        when (citing `S.notMember` nodesWithItems)
            $ tell [showNode citing++" has arc yet has no items"]

type ModelInit = Map CitingNodeItem (Setting CitingUpdateUnit)

modify' :: Monad m => (a -> a) -> StateT a m ()
modify' f = do x <- get
               put $! f x

randomInitializeCiting :: NetData -> ModelInit -> RVar ModelInit
randomInitializeCiting d init = execStateT doInit init
    where doInit :: StateT ModelInit RVar ()
          doInit = let unset = M.keysSet (dCitingNodeItems d) `S.difference` M.keysSet init
                   in mapM_ (randomInitCitingUU d) (S.toList unset)

randomInitCitingUU :: NetData -> CitingNodeItem -> StateT ModelInit RVar ()
randomInitCitingUU d cni@(Citing ni) =
    let (n,_) = dNodeItems d M.! ni
    in case getCitedNodes d (Citing n) of
           a | S.null a -> do
               modify' $ M.insert cni OwnSetting

           citedNodes -> do
               s <- lift $ randomElement [Shared, Own]
               c <- lift $ randomElement $ toList citedNodes
               modify' $ M.insert cni $
                   case s of Shared -> SharedSetting c
                             Own    -> OwnSetting

randomInitialize :: NetData -> RVar ModelInit
randomInitialize d = randomInitializeCiting d M.empty

model :: NetData -> ModelInit -> MState
model d citingInit =
    let citingNodes = dCitingNodes d
        s = MState { -- Citing model
                     stPsis = let dist n = case toList $ getCitedNodes d n of
                                               []    -> M.empty
                                               nodes -> M.singleton n
                                                        $ symDirMulti (dAlphaPsi d) nodes
                              in foldMap dist citingNodes
                   , stGammas = let dist = multinom [ (Shared, dAlphaGammaShared d)
                                                    , (Own, dAlphaGammaOwn d) ]
                                in foldMap (\t->M.singleton t dist) citingNodes
                   , stOmegas = let dist = symDirMulti (dAlphaOmega d) (M.keys $ dItems d)
                                in foldMap (\t->M.singleton t dist) citingNodes
                   , stCiting = M.empty

                   -- Cited model
                   , stLambdas = let dist = symDirMulti (dAlphaLambda d) (M.keys $ dItems d)
                                 in foldMap (\t->M.singleton t dist) $ dCitedNodes d
                   }

        initCitingUU :: CitingUpdateUnit -> State MState ()
        initCitingUU uu = do
            let err = error $ "CitationInference: Initial value for "++show uu++" not given\n"
                s = maybe err id $ M.lookup (uuNI uu) citingInit
            modify' $ setCitingUU uu (Just s)

    in execState (mapM_ initCitingUU $ citingUpdateUnits d) s

updateUnits :: NetData -> [WrappedUpdateUnit MState]
updateUnits d = map WrappedUU (citingUpdateUnits d)

data CitingSetting = OwnSetting
                   | SharedSetting !CitedNode
                   deriving (Show, Eq, Generic)
instance Binary CitingSetting
instance NFData CitingSetting where
    rnf (OwnSetting)      = ()
    rnf (SharedSetting c) = rnf c `seq` ()

data MState = MState { -- Citing model state
                       stGammas   :: !(Map CitingNode (Multinom Int ItemSource))
                     , stOmegas   :: !(Map CitingNode (Multinom Int Item))
                     , stPsis     :: !(Map CitingNode (Multinom Int CitedNode))

                     , stCiting   :: !(Map CitingNodeItem CitingSetting)

                     -- Cited model state
                     , stLambdas  :: !(Map CitedNode (Multinom Int Item))
                     }
            deriving (Show, Generic)
instance Binary MState

modelLikelihood :: MState -> Probability
modelLikelihood model =
    product $ map likelihood (M.elems $ stGammas model)
           ++ map likelihood (M.elems $ stLambdas model)
           ++ map likelihood (M.elems $ stOmegas model)
           ++ map likelihood (M.elems $ stPsis model)

-- Citing Update unit (Shared Taste-like)
data CitingUpdateUnit = CitingUpdateUnit { uuNI    :: CitingNodeItem
                                         , uuN     :: CitingNode
                                         , uuX     :: Item
                                         , uuCites :: Set CitedNode
                                         }
                      deriving (Show, Generic)
instance Binary CitingUpdateUnit

instance UpdateUnit CitingUpdateUnit where
    type ModelState CitingUpdateUnit = MState
    type Setting CitingUpdateUnit = CitingSetting
    fetchSetting uu ms = stCiting ms M.! uuNI uu
    evolveSetting ms uu = categorical $ citingFullCond (setCitingUU uu Nothing ms) uu
    updateSetting uu _ s' = setCitingUU uu (Just s') . setCitingUU uu Nothing

citingUpdateUnits :: NetData -> [CitingUpdateUnit]
citingUpdateUnits d =
    map (\(ni,(n,x))->CitingUpdateUnit { uuNI      = ni
                                       , uuN       = n
                                       , uuX       = x
                                       , uuCites   = getCitedNodes d n
                                       }
        ) $ M.assocs $ dCitingNodeItems d

citingProb :: MState -> CitingUpdateUnit -> Setting CitingUpdateUnit -> Double
citingProb st (CitingUpdateUnit {uuN=n, uuX=x}) setting =
    let gamma = stGammas st M.! n
        omega = stOmegas st M.! n
        psi = stPsis st M.! n
    in case setting of
        SharedSetting c   -> let lambda = stLambdas st M.! c
                             in sampleProb gamma Shared
                              * sampleProb psi c
                              * sampleProb lambda x
        OwnSetting        ->  sampleProb gamma Own
                              * sampleProb omega x

citingFullCond :: MState -> CitingUpdateUnit -> [(Double, Setting CitingUpdateUnit)]
citingFullCond ms uu = map (\s->(citingProb ms uu s, s)) $ citingDomain ms uu

citingDomain :: MState -> CitingUpdateUnit -> [Setting CitingUpdateUnit]
citingDomain ms uu = do
    s <- [Own, Shared]
    case s of
        Shared -> do c <- S.toList $ uuCites uu
                     return $ SharedSetting c
        Own    -> do return $ OwnSetting

setCitingUU :: CitingUpdateUnit -> Maybe (Setting CitingUpdateUnit) -> MState -> MState
setCitingUU uu@(CitingUpdateUnit {uuNI=ni, uuN=n, uuX=x}) setting ms =
    let set = maybe Unset (const Set) setting
        ms' = case maybe (fetchSetting uu ms) id  setting of
            SharedSetting c   -> ms { stPsis = M.adjust (setMultinom set c) n $ stPsis ms
                                    , stLambdas = M.adjust (setMultinom set x) c $ stLambdas ms
                                    , stGammas = M.adjust (setMultinom set Shared) n $ stGammas ms
                                    }
            OwnSetting        -> ms { stOmegas = M.adjust (setMultinom set x) n $ stOmegas ms
                                    , stGammas = M.adjust (setMultinom set Own) n $ stGammas ms
                                    }
    in ms' { stCiting = M.alter (const setting) ni $ stCiting ms' }