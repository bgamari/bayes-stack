{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric, RecordWildCards #-}

module BayesStack.Models.Topic.SharedTaste
  ( -- * Primitives
    NetData(..)
  , HyperParams(..)
  , MState(..)
  , STUpdateUnit
  , ItemSource(..)
  , Node(..), Item(..), Topic(..), Edge(..)
  , NodeItem(..), setupNodeItems
    -- * Initialization
  , ModelInit
  , randomInitialize
  , model, updateUnits
    -- * Diagnostics
  , modelLikelihood
  , influence
  ) where

import Prelude hiding (mapM, sum)

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
import Numeric.Log hiding (sum)

import BayesStack.Types
import BayesStack.Gibbs
import BayesStack.DirMulti
import BayesStack.TupleEnum ()
import BayesStack.Models.Topic.Types

import GHC.Generics
import Data.Binary

data ItemSource = Shared | Own
                deriving (Show, Eq, Generic, Enum, Ord)
instance Binary ItemSource
instance NFData ItemSource

data HyperParams = HyperParams
                   { alphaPsi         :: Double
                   , alphaLambda      :: Double
                   , alphaPhi         :: Double
                   , alphaOmega       :: Double
                   , alphaGammaShared :: Double
                   , alphaGammaOwn    :: Double
                   }
                 deriving (Show, Eq, Generic)
instance Binary HyperParams     

data NetData = NetData { dHypers             :: !HyperParams
                       , dEdges              :: !(Set Edge)
                       , dItems              :: !(Set Item)
                       , dTopics             :: !(Set Topic)
                       , dNodeItems          :: !(Map NodeItem (Node, Item))
                       }
             deriving (Show, Eq, Generic)
instance Binary NetData

dNodes :: NetData -> Set Node
dNodes = S.fromList . map fst . M.elems . dNodeItems

dAdjNodes :: NetData -> Node -> Set Node
dAdjNodes nd n = S.fromList $ getFriends (S.toList $ dEdges nd) n

dItemsOfNode :: NetData -> Node -> [Item]
dItemsOfNode nd u = map snd $ filter (\(n,_)->u==n) $ M.elems $ dNodeItems nd

type ModelInit = Map NodeItem (Setting STUpdateUnit)

randomInit :: NetData -> Map Node (Set Node) -> NodeItem -> RVar ModelInit
randomInit d friends ni = do
    let topics = S.toList $ dTopics d
    t <- randomElement topics
    s <- randomElement [Shared, Own]
    let (u,_) = dNodeItems d M.! ni
    f <- randomElement $ S.toList
         $ maybe (error "SharedTaste.randomInit: No friends") id $ M.lookup u friends
    return $ M.singleton ni $
        case s of Shared -> SharedSetting t (Edge (u,f))
                  Own    -> OwnSetting t

randomInitialize' :: NetData -> ModelInit -> RVar ModelInit
randomInitialize' d init =
  let unset = M.keysSet (dNodeItems d) `S.difference` M.keysSet init
      friends = M.unionsWith S.union $ map (\(Edge (a,b))-> M.singleton a (S.singleton b)
                                                         <> M.singleton b (S.singleton a)
                                           ) $ S.toList $ dEdges d
  in liftM mconcat $ forM (S.toList unset) $ randomInit d friends


randomInitialize :: NetData -> RVar ModelInit
randomInitialize = (flip randomInitialize') M.empty

updateUnits' :: NetData -> [STUpdateUnit]
updateUnits' d =
    map (\(ni,(n,x)) ->
               STUpdateUnit { uuNI      = ni
                            , uuN       = n
                            , uuX       = x
                            , uuFriends = getFriends (S.toList $ dEdges d) n
                            }
        )
    $ M.assocs $ dNodeItems d

updateUnits :: NetData -> [WrappedUpdateUnit MState]
updateUnits = map WrappedUU . updateUnits'

model :: NetData -> ModelInit -> MState
model d init =
    let uus = updateUnits' d
        s = MState { stPsis = let dist n = symDirMulti alphaPsi (toList $ getFriends (toList $ dEdges d) n)
                               in foldMap (\n->M.singleton n $ dist n) $ dNodes d
                    , stPhis = let dist = symDirMulti alphaPhi (toList $ dItems d)
                               in foldMap (\t->M.singleton t dist) $ dTopics d
                    , stGammas = let dist = multinom [ (Shared, alphaGammaShared)
                                                     , (Own, alphaGammaOwn) ]
                                 in foldMap (\t->M.singleton t dist) $ dNodes d
                    , stOmegas = let dist = symDirMulti alphaOmega (toList $ dTopics d)
                                 in foldMap (\t->M.singleton t dist) $ dNodes d
                    , stLambdas = let dist = symDirMulti alphaLambda (toList $ dTopics d)
                                  in foldMap (\t->M.singleton t dist) $ dEdges d
                    , stVars = M.empty
                    }
        HyperParams {..} = dHypers d
        initUU uu = do
            let s = maybe (error "Incomplete initialization") id $ M.lookup (uuNI uu) init
            modify $ setUU uu (Just s)
    in execState (mapM initUU uus) s

data STSetting = OwnSetting !Topic
               | SharedSetting !Topic !Edge
               deriving (Show, Eq, Generic)

instance Binary STSetting
instance NFData STSetting where
    rnf (OwnSetting t)      = rnf t `seq` ()
    rnf (SharedSetting t f) = rnf t `seq` rnf f `seq` ()

data MState = MState { stGammas   :: !(Map Node (Multinom Int ItemSource))
                     , stOmegas   :: !(Map Node (Multinom Int Topic))
                     , stPsis     :: !(Map Node (Multinom Int Node))
                     , stLambdas  :: !(Map Edge (Multinom Int Topic))
                     , stPhis     :: !(Map Topic (Multinom Int Item))

                     , stVars     :: !(Map NodeItem STSetting)
                     }
            deriving (Show, Generic)
instance Binary MState

data STUpdateUnit = STUpdateUnit { uuNI      :: NodeItem
                                 , uuN       :: Node
                                 , uuX       :: Item
                                 , uuFriends :: [Node]
                                 }
                   deriving (Show, Generic)
instance Binary STUpdateUnit

setUU :: STUpdateUnit -> Maybe (Setting STUpdateUnit) -> MState -> MState
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
    type ModelState STUpdateUnit = MState
    type Setting STUpdateUnit = STSetting
    fetchSetting uu ms = stVars ms M.! uuNI uu
    evolveSetting ms uu = categorical $ stFullCond (setUU uu Nothing ms) uu
    updateSetting uu _ s' = setUU uu (Just s') . setUU uu Nothing

uuProb :: MState -> STUpdateUnit -> Setting STUpdateUnit -> Double
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

stFullCond :: MState -> STUpdateUnit -> [(Double, Setting STUpdateUnit)]
stFullCond ms uu = map (\s->(uuProb ms uu s, s)) $ stDomain ms uu

stDomain :: MState -> STUpdateUnit -> [Setting STUpdateUnit]
stDomain ms uu = do
    s <- [Own, Shared]
    t <- M.keys $ stPhis ms
    case s of
        Shared -> do f <- uuFriends uu
                     return $ SharedSetting t (Edge (uuN uu, f))
        Own    -> do return $ OwnSetting t

modelLikelihood :: MState -> Probability
modelLikelihood model =
    product (map likelihood $ M.elems $ stGammas model)
  * product (map likelihood $ M.elems $ stPhis model)
  * product (map likelihood $ M.elems $ stLambdas model)
  * product (map likelihood $ M.elems $ stOmegas model)
  * product (map likelihood $ M.elems $ stPsis model)

-- | The probability of a collections of items under a given topic mixture.
topicCompatibility :: MState -> [Item] -> Multinom Int Topic -> Probability
topicCompatibility m items lambda =
    product $ do t <- toList $ dmDomain lambda
                 let phi = stPhis m M.! t
                     itemObs = zip items (repeat 1)
                 return $ realToFrac (sampleProb lambda t) * obsProb phi itemObs

topicCompatibilities :: (Functor f, Foldable f)
                     => MState -> [Item] -> f (Multinom Int Topic) -> f Probability
topicCompatibilities m items topics =
    let scores = fmap (topicCompatibility m items) topics
    in fmap (/sum scores) scores

-- | The influence of adjacent nodes on a node.
influence :: NetData -> MState -> Node -> Map Node Probability
influence d m u =
    let lambdas = foldMap (\f->M.singleton f $ stLambdas m M.! Edge (u,f))
                  $ dAdjNodes d u
    in topicCompatibilities m (dItemsOfNode d u) lambdas
