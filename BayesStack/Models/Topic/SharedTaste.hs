{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module BayesStack.Models.Topic.SharedTaste
  ( -- * Primitives
    STData(..)
  , ItemSource(..)
  , Node(..), Item(..), Topic(..)
  , NodeItem, setupNodeItems
  , Friendship(..), otherFriend, isFriend, getFriends
    -- * Initialization
  , ModelInit
  , randomInitialize, smartInitialize
    -- * Model
  , STModel(..), ItemUnit
  , model, likelihood
  , STModelState (..), getModelState
  , sortTopics
  , ItemVars(..)
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
import Data.List (sortBy)

import Control.Monad (liftM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

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

import Data.Serialize (Serialize)
import GHC.Generics

data ItemSource = Shared | Own deriving (Show, Eq, Generic, Enum, Ord)
instance Serialize ItemSource

data STData = STData { stAlphaGammaShared, stAlphaGammaOwn :: Double
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

data ItemVars = ItemVars { ivS :: !ItemSource
                         , ivF :: !Node
                         , ivT :: !Topic
                         }
                deriving (Show, Eq, Generic)
instance Serialize ItemVars

data STModel = STModel { mData :: STData
                       , mGammas :: SharedEnumMap Node (DirMulti ItemSource)
                       , mOmegas :: SharedEnumMap Node (DirMulti Topic)
                       , mPsis :: SharedEnumMap Node (DirMulti Node)
                       , mLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                       , mPhis :: SharedEnumMap Topic (DirMulti Item)
                       , mVars :: SharedEnumMap NodeItem ItemVars
                       , mSortedTopics :: SharedEnumMap Item [Topic]
                       }

data STModelState = STModelState { msData :: STData
                                 , msGammas :: EnumMap Node (DirMulti ItemSource)
                                 , msOmegas :: EnumMap Node (DirMulti Topic)
                                 , msPsis :: EnumMap Node (DirMulti Node)
                                 , msLambdas :: EnumMap Friendship (DirMulti Topic)
                                 , msPhis :: EnumMap Topic (DirMulti Item)
                                 , msVars :: EnumMap NodeItem ItemVars
                                 , msLogLikelihood :: Double
                                 } deriving (Show, Generic)
instance Serialize STModelState

type ModelInit = EnumMap NodeItem ItemVars

randomInitialize' :: STData -> ModelInit -> RVar ModelInit
randomInitialize' d init = 
  let unset = EM.keysSet (stNodeItems d) `ES.difference` EM.keysSet init
      topics = S.toList $ stTopics d
      randomInit :: NodeItem -> RVar ModelInit
      randomInit ni = do t <- randomElement topics
                         let (n,_) = EM.findWithDefault (error "Can't find nodeItem") ni (stNodeItems d)
                             friends = getFriends (S.toList $ stFriendships d) n
                         f <- randomElement friends
                         --s <- bernoulli (0.5::Double)
                         let s = Shared -- FIXME
                         return $ EM.singleton ni $ ItemVars s f t
  in liftM ((init `mappend`) . mconcat) $ forM (ES.toList unset) randomInit

randomInitialize :: STData -> RVar ModelInit
randomInitialize = (flip randomInitialize') EM.empty

smartInitialize :: STData -> RVar ModelInit
smartInitialize d =
  let STData {stTopics=topics, stNodes=nodes, stItems=items, stNodeItems=nodeItems} = d
      STData {stFriendships=friendships, stNodeItems=nis} = d
      nisInv :: EnumMap (Node,Item) [NodeItem]
      nisInv = EM.fromListWith (++) $ map (\(ni,(n,i))->((n,i),[ni])) $ EM.toList nis

      sharedTopics :: Friendship -> StateT (EnumMap Item Topic, EnumMap Friendship (Set Topic)) RVar ModelInit
      sharedTopics fs@(Friendship (a,b)) =
        let findItems n = S.fromList $ map snd $ filter (\(n',i)->n==n') $ toList nodeItems
            sharedItems = toList $ S.intersection (findItems a) (findItems b)
        in liftM mconcat $ forM sharedItems $ \x -> do
             (itemTopics,_) <- get
             t <- if x `EM.member` itemTopics
                    then return $ itemTopics EM.! x
                    else do (_, friendshipTopics) <- get
                            --let possTopics = EM.findWithDefault topics fs friendshipTopics
                            let possTopics = topics -- FIXME
                            t <- lift $ randomElement $ S.toList possTopics
                            modify $ \(a,b)->(EM.insert x t a, b)
                            return t
             modify $ \(a,b)->(a, EM.insertWith S.union fs (S.singleton t) b)
             return $ EM.fromList $ do ax <- EM.findWithDefault (error "fuck") (a,x) nisInv --nisInv EM.! (a,x)
                                       return (ax, ItemVars Shared b t)
                                 ++ do bx <- EM.findWithDefault (error "fuck2") (b,x) nisInv -- nisInv EM.! (b,x)
                                       return (bx, ItemVars Shared a t)
  in do a <- evalStateT (mapM sharedTopics $ S.toList friendships) (EM.empty, EM.empty)
        randomInitialize' d $ mconcat a

data ItemUnit = ItemUnit { iuModel :: STModel
                         , iuNodeItem :: NodeItem
                         , iuFriends :: Set Node
                         , iuN :: Node
                         , iuVars :: Shared ItemVars
                         , iuX :: Item
                         , iuGamma :: Shared (DirMulti ItemSource)
                         , iuOmega :: Shared (DirMulti Topic)
                         , iuLambdas :: SharedEnumMap Friendship (DirMulti Topic)
                         , iuPhis :: SharedEnumMap Topic (DirMulti Item)
                         , iuState :: Shared GibbsUpdateState
                         }

model :: STData -> ModelInit -> ModelMonad (Seq ItemUnit, STModel)
model d init =
  do let STData {stTopics=topics, stNodes=nodes, stItems=items, stNodeItems=nis} = d
         STData {stFriendships=friendships} = d
         friends :: EnumMap Node (Set Node)
         friends = foldMap (\n->EM.singleton n $ S.fromList $ getFriends (S.toList friendships) n) nodes
     gammas <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ fixedDirMulti [ (Shared, stAlphaGammaShared d)
                              , (Own, stAlphaGammaOwn d) ]
     omegas <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaOmega d) (S.toList topics)
     psis <- newSharedEnumMap (S.toList nodes) $ \n ->
       return $ symDirMulti (stAlphaPsi d) (S.toList nodes)
     lambdas <- newSharedEnumMap (S.toList friendships) $ \n ->
       return $ symDirMulti (stAlphaLambda d) (S.toList topics)
     phis <- newSharedEnumMap (S.toList topics) $ \t ->
       return $ symDirMulti (stAlphaPhi d) (S.toList items)

     ivs <- newSharedEnumMap (EM.keys nis) $ \ni ->
       return $ EM.findWithDefault (error "Incomplete initialization") ni init
  
     sortedTopics <- newSharedEnumMap (S.toList items) $ \x -> return $ S.toList topics

     let model = STModel { mData = d
                         , mGammas = gammas
                         , mOmegas = omegas
                         , mPsis = psis
                         , mLambdas = lambdas
                         , mPhis = phis
                         , mVars = ivs
                         , mSortedTopics = sortedTopics
                         }

     itemUnits <- forM (EM.toList ivs) $ \(ni,iv) ->
       do state <- newGibbsUpdateState
          let (n,x) = nis EM.! ni
          let unit = ItemUnit { iuModel = model
                              , iuNodeItem = ni
                              , iuFriends = friends EM.! n
                              , iuN = n
                              , iuVars = iv
                              , iuX = x
                              , iuGamma = gammas EM.! n
                              , iuOmega = omegas EM.! n
                              , iuLambdas = EM.filterWithKey (\k _->isFriend n k) lambdas
                              , iuPhis = phis
                              , iuState = state
                              }
          getShared iv >>= guSet unit
          return unit
     return (SQ.fromList itemUnits, model)

sortTopics :: STModel -> ModelMonad ()
sortTopics model =
  forM_ (EM.toList $ mSortedTopics model) $ \(x,topics)->do
    d <- getShared topics
    weights <- forM d $ \t->do phi <- getShared $ mPhis model EM.! t
                               return $ prob phi x
    setShared topics $ map snd $ sortBy (flip (compare `on` fst)) $ zip weights d

likelihood :: STModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ stNodeItems $ mData model) $ \(ni, (n,x)) ->
       do ItemVars s f t <- getShared $ mVars model EM.! ni
          gamma <- getShared $ mGammas model EM.! n
          omega <- getShared $ mOmegas model EM.! n
          psi <- getShared $ mPsis model EM.! n
          lambda <- getShared $ mLambdas model EM.! Friendship (n,f)
          phi <- getShared $ mPhis model EM.! t
          case s of
              Shared -> return $ Product $ logFloat (prob gamma s)
                                         * logFloat (prob psi f)
                                         * logFloat (prob lambda t)
                                         * logFloat (prob phi x)
              Own -> return $ Product $ logFloat (prob gamma s)
                                      * logFloat (prob omega t)
                                      * logFloat (prob phi x)
     return $ getProduct $ mconcat a

instance GibbsUpdateUnit ItemUnit where
  type GUValue ItemUnit = ItemVars
  guProb unit (ItemVars s f t) =
    do gamma <- getShared $ iuGamma unit
       omega <- getShared $ iuOmega unit
       psi <- getShared $ mPsis (iuModel unit) EM.! iuN unit
       phi <- getShared $ iuPhis unit EM.! t 
       lambda <- getShared $ iuLambdas unit EM.! Friendship (iuN unit, f)
       case s of
            Shared -> return $ prob gamma s * prob psi f * prob lambda t * prob phi (iuX unit) 
            Own -> return $ prob gamma s * prob omega t * prob phi (iuX unit)
  
  guDomain unit = return $ (do t <- S.toList $ stTopics $ mData $ iuModel unit
                               f <- S.toList $ iuFriends unit
                               return $ ItemVars Shared f t)
                        ++ (do t <- S.toList $ stTopics $ mData $ iuModel unit
                               let f = head $ S.toList $ iuFriends unit
                               return $ ItemVars Own f t)
  
  guUnset unit =
    do ItemVars s f t <- getShared $ iuVars unit
       let x = iuX unit
           u = iuN unit
           m = iuModel unit
           gamma = iuGamma unit
           omega = iuOmega unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       gamma `updateShared` decDirMulti s
       case s of
            Shared -> do (mPsis m EM.! u) `updateShared` decDirMulti f
                         (mPsis m EM.! f) `updateShared` decDirMulti u
                         lambda `updateShared` decDirMulti t
            Own -> omega `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
       return $ ItemVars s f t
  
  guSet unit iv@(ItemVars s f t) =
    do iuVars unit `setShared` iv
       let x = iuX unit
           u = iuN unit
           m = iuModel unit
           gamma = iuGamma unit
           omega = iuOmega unit
           lambda = iuLambdas unit EM.! Friendship (iuN unit, f)
           phi = iuPhis unit EM.! t
       gamma `updateShared` incDirMulti s
       case s of
            Shared -> do (mPsis m EM.! u) `updateShared` incDirMulti f
                         (mPsis m EM.! f) `updateShared` incDirMulti u
                         lambda `updateShared` incDirMulti t
            Own -> omega `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x

  guState = iuState

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

