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

import Control.DeepSeq
import Control.Monad (liftM)
import Control.Monad.Trans.State
import Data.Random
import Data.Random.Distribution.Categorical (categorical)
import Data.Number.LogFloat hiding (realToFrac)

import BayesStack.Core.Types
import BayesStack.Core.Gibbs
import BayesStack.DirMulti
import BayesStack.TupleEnum ()
import BayesStack.Models.Topic.Types

import GHC.Generics
import Data.Serialize

data ItemSource = Shared | Own
                deriving (Show, Eq, Generic, Enum, Ord)
instance Serialize ItemSource
instance NFData ItemSource         

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

randomInit :: STData -> NodeItem -> RVar ModelInit
randomInit d ni = do
    let topics = S.toList $ stTopics d
    t <- randomElement topics
    s <- randomElement [Shared, Own]
    let (u,_) = stNodeItems d M.! ni
    f <- randomElement $ getFriends (S.toList $ stFriendships d) u
    return $ M.singleton ni $
        case s of Shared -> SharedSetting t (Friendship (u,f))
                  Own    -> OwnSetting t

randomInitialize' :: STData -> ModelInit -> RVar ModelInit
randomInitialize' d init = 
  let unset = M.keysSet (stNodeItems d) `S.difference` M.keysSet init
  in liftM mconcat $ forM (S.toList unset) $ randomInit d


randomInitialize :: STData -> RVar ModelInit
randomInitialize = (flip randomInitialize') M.empty
                
updateUnits' :: STData -> [STUpdateUnit]
updateUnits' d =
    map (\(ni,(n,x)) ->
               STUpdateUnit { uuNI      = ni
                            , uuN       = n
                            , uuX       = x
                            , uuFriends = getFriends (S.toList $ stFriendships d) n
                            }
        )
    $ M.assocs $ stNodeItems d

updateUnits :: STData -> [WrappedUpdateUnit STState]
updateUnits = map WrappedUU . updateUnits'            
              
model :: STData -> ModelInit -> STState
model d init =
    let uus = updateUnits' d
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
                    , stVars = M.empty
                    }
        initUU uu = do
            let s = M.findWithDefault (error "Incomplete initialization") (uuNI uu) init
            modify $ setUU uu (Just s)
    in execState (mapM initUU uus) s

data STSetting = OwnSetting !Topic
               | SharedSetting !Topic !Friendship
               deriving (Show, Eq, Generic)

instance Serialize STSetting
instance NFData STSetting where
    rnf (OwnSetting t)      = rnf t `seq` ()
    rnf (SharedSetting t f) = rnf t `seq` rnf f `seq` ()

data STState = STState { stGammas   :: Map Node (Multinom ItemSource)
                       , stOmegas   :: Map Node (Multinom Topic)
                       , stPsis     :: Map Node (Multinom Node)
                       , stLambdas  :: Map Friendship (Multinom Topic)
                       , stPhis     :: Map Topic (Multinom Item)

                       , stVars     :: Map NodeItem STSetting
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
setUU uu@(STUpdateUnit {uuNI=ni, uuN=n, uuX=x}) setting ms =
    let set = maybe Unset (const Set) setting
        ms' = case maybe (fetchSetting uu ms) id  setting of
            SharedSetting t fship ->
                let f = maybe (error "Node isn't part of friendship") id
                        $ otherFriend n fship
                in ms { stPsis = M.adjust (setMultinom set n) f
                                 $ M.adjust (setMultinom set f) n $ stPsis ms
                      , stLambdas = M.adjust (setMultinom set t) fship $ stLambdas ms
                      , stPhis = M.adjust (setMultinom set x) t $ stPhis ms
                      , stGammas = M.adjust (setMultinom set Shared) n $ stGammas ms
                      }
            OwnSetting t ->
                ms { stOmegas = M.adjust (setMultinom set t) n $ stOmegas ms
                   , stPhis = M.adjust (setMultinom set x) t $ stPhis ms
                   , stGammas = M.adjust (setMultinom set Own) n $ stGammas ms
                   }
    in ms' { stVars = M.alter (const setting) ni $ stVars ms' }

instance UpdateUnit STUpdateUnit where
    type ModelState STUpdateUnit = STState
    type Setting STUpdateUnit = STSetting
    fetchSetting uu ms = stVars ms M.! uuNI uu
    evolveSetting ms uu = categorical $ stFullCond (setUU uu Nothing ms) uu
    updateSetting uu _ s' = setUU uu (Just s') . setUU uu Nothing
        
uuProb :: STState -> STUpdateUnit -> Setting STUpdateUnit -> Double
uuProb st (STUpdateUnit {uuN=n, uuX=x}) setting =
    let gamma = stGammas st M.! n
        omega = stOmegas st M.! n
        psi = stPsis st M.! n
    in case setting of 
        SharedSetting t fship -> let phi = stPhis st M.! t
                                     lambda = stLambdas st M.! fship
                                     f = maybe (error "Friend isn't friends with node") id
                                         $ otherFriend n fship
                                 in sampleProb gamma Shared
                                  * sampleProb psi f
                                  * sampleProb lambda t
                                  * sampleProb phi x
        OwnSetting t -> let phi = stPhis st M.! t
                        in sampleProb gamma Own
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
                     return $ SharedSetting t (Friendship (uuN uu, f))
        Own    -> do return $ OwnSetting t

modelLikelihood :: STState -> Probability
modelLikelihood model =
    product $ map likelihood (M.elems $ stGammas model)
           ++ map likelihood (M.elems $ stPhis model)
           ++ map likelihood (M.elems $ stLambdas model)
           ++ map likelihood (M.elems $ stOmegas model)
           ++ map likelihood (M.elems $ stPsis model)
