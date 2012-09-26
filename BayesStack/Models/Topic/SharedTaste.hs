{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.SharedTaste
  ( -- * Primitives
    STData(..)
  , STState(..)
  , STUpdateUnit
  , ItemSource(..)
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
import Data.Number.LogFloat hiding (realToFrac)

import BayesStack.Core.Types
import BayesStack.Core.Gibbs
import BayesStack.DirMulti
import BayesStack.TupleEnum
import BayesStack.Models.Topic.Types

import GHC.Generics
import Data.Serialize

data ItemSource = Shared | Own deriving (Show, Eq, Generic, Enum, Ord)
instance Serialize ItemSource

data STData = STData { stAlphaPsi           :: Double
                     , stAlphaLambda        :: Double
                     , stAlphaPhi           :: Double
                     , stAlphaOmega         :: Double
                     , stAlphaGammaShared   :: Double
                     , stAlphaGammaOwn      :: Double
                     , stNodes              :: Set Node
                     , stFriendships        :: Set Friendship
                     , stItems              :: Set Item
                     , stTopics             :: Set Topic
                     , stNodeItems          :: Map NodeItem (Node, Item)
                     }
             deriving (Show, Eq, Generic)
instance Serialize STData

type ModelInit = Map NodeItem (Setting STUpdateUnit)

randomInitialize' :: STData -> ModelInit -> RVar ModelInit
randomInitialize' d init = 
  let unset = M.keysSet (stNodeItems d) `S.difference` M.keysSet init
      topics = S.toList $ stTopics d
      randomInit :: NodeItem -> RVar ModelInit
      randomInit ni = do t <- randomElement topics
                         s <- randomElement [Shared, Own]
                         let (u,_) = stNodeItems d M.! ni
                         f <- randomElement $ getFriends (S.toList $ stFriendships d) u
                         return $ M.singleton ni (s,f,t)
  in liftM mconcat $ forM (S.toList unset) randomInit

randomInitialize :: STData -> RVar ModelInit
randomInitialize = (flip randomInitialize') M.empty
                
updateUnits :: STData -> [STUpdateUnit]
updateUnits d =
    map (\(ni,(n,x))->STUpdateUnit { uuNI      = ni
                                   , uuN       = n
                                   , uuX       = x
                                   , uuFriends = getFriends (S.toList $ stFriendships d) n
                                   }
        )
    $ M.assocs $ stNodeItems d
              
model :: STData -> ModelInit -> STState
model d init =
    let uus = updateUnits d
        s = STState { stPsis = let dist = symDirMulti (stAlphaPsi d) (toList $ stNodes d)
                               in foldMap (\n->M.singleton n dist) $ stNodes d
                    , stPhis = let dist = symDirMulti (stAlphaPhi d) (toList $ stItems d)
                               in foldMap (\t->M.singleton t dist) $ stTopics d
                    , stGammas = let dist = multinom [ (Shared, stAlphaGammaShared d)
                                                     , (Own, stAlphaGammaOwn d) ]
                                 in foldMap (\t->M.singleton t dist) $ stNodes d
                    , stOmegas = let dist = symDirMulti (stAlphaOmega d) (toList $ stTopics d)
                                 in foldMap (\t->M.singleton t dist) $ stNodes d
                    , stLambdas = let dist = symDirMulti (stAlphaLambda d) (toList $ stTopics d)
                                  in foldMap (\t->M.singleton t dist) $ stFriendships d
                    , stS = M.empty
                    , stT = M.empty
                    , stF = M.empty
                    }
        initUU uu = do
            let s = M.findWithDefault (error "Incomplete initialization") (uuNI uu) init
            modify $ setUU uu (Just s)
    in execState (mapM initUU uus) s

data STState = STState { stGammas   :: Map Node (Multinom ItemSource)
                       , stOmegas   :: Map Node (Multinom Topic)
                       , stPsis     :: Map Node (Multinom Node)
                       , stLambdas  :: Map Friendship (Multinom Topic)
                       , stPhis     :: Map Topic (Multinom Item)

                       , stS        :: Map NodeItem ItemSource
                       , stT        :: Map NodeItem Topic
                       , stF        :: Map NodeItem Node
                       }
              deriving (Show, Generic)
instance Serialize STState

data STUpdateUnit = STUpdateUnit { uuNI :: NodeItem
                                 , uuN  :: Node
                                 , uuX  :: Item
                                 , uuFriends :: [Node]
                                 }
                   deriving (Show, Generic)
instance Serialize STUpdateUnit

setUU :: STUpdateUnit -> Maybe (Setting STUpdateUnit) -> STState -> STState
setUU uu@(STUpdateUnit {uuN=n, uuNI=ni, uuX=x}) setting ms =
    let set = maybe Unset (const Set) setting
        (s,f,t) = maybe (fetchSetting uu ms) id setting
        friendship = Friendship (n,f)
        ms' = case stS ms M.! ni of
            Shared -> ms { stPsis = M.adjust (setMultinom set f) n
                                  $ M.adjust (setMultinom set n) f
                                  $ stPsis ms
                         , stLambdas = M.adjust (setMultinom set t) friendship (stLambdas ms)
                         }
            Own    -> ms { stOmegas = M.adjust (setMultinom set t) n (stOmegas ms) }
    in ms' { stPhis = M.adjust (setMultinom set x) t (stPhis ms)
           , stGammas = M.adjust (setMultinom set s) n (stGammas ms)
           , stS = case setting of Just _  -> M.insert ni s $ stS ms
                                   Nothing -> error "Unset S"
           , stF = case setting of Just _  -> M.insert ni f $ stF ms
                                   Nothing -> error "Unset F"
           , stT = case setting of Just _  -> M.insert ni t $ stT ms
                                   Nothing -> error "Unset T"
           }

instance UpdateUnit STUpdateUnit where
    type ModelState STUpdateUnit = STState
    type Setting STUpdateUnit = (ItemSource, Node, Topic)
    fetchSetting uu ms = ( stS ms M.! uuNI uu
                         , stF ms M.! uuNI uu
                         , stT ms M.! uuNI uu
                         )
    evolveSetting ms uu = categorical $ stFullCond (setUU uu Nothing ms) uu
    updateSetting uu s s' = setUU uu (Just s') . setUU uu Nothing
        
uuProb :: STState -> STUpdateUnit -> Setting STUpdateUnit -> Double
uuProb st (STUpdateUnit {uuNI=ni, uuN=n, uuX=x}) (s,f,t) =
    let gamma = stGammas st M.! n
        omega = stOmegas st M.! n
        phi = stPhis st M.! t
        psi = stPsis st M.! n
        lambda = stLambdas st M.! Friendship (n,f)
    in case s of 
        Shared ->   sampleProb gamma s
                  * sampleProb psi f
                  * sampleProb lambda t
                  * sampleProb phi x
        Own ->   sampleProb gamma s
               * sampleProb omega t
               * sampleProb phi x

stFullCond :: STState -> STUpdateUnit -> [(Double, Setting STUpdateUnit)]
stFullCond ms uu = map (\s->(uuProb ms uu s, s)) $ stDomain ms uu
            
stDomain :: STState -> STUpdateUnit -> [Setting STUpdateUnit]
stDomain ms uu = do
    s <- [Own, Shared]
    t <- M.keys $ stPhis ms
    case s of
        Shared -> do f <- uuFriends uu
                     return (Shared, f, t)
        Own    -> do return (Own, error "No friend for Own item", t)

modelLikelihood :: STState -> Probability
modelLikelihood model =
    product $ map likelihood (M.elems $ stGammas model)
           ++ map likelihood (M.elems $ stPhis model)
           ++ map likelihood (M.elems $ stLambdas model)
           ++ map likelihood (M.elems $ stOmegas model)
           ++ map likelihood (M.elems $ stPsis model)
