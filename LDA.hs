{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module LDA ( LDAData(..)
           , Node(..), Item(..), Topic(..)
           , LDAModel(..), ItemUnit
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

import Control.Monad (when)
import Control.Monad.IO.Class

data LDAData = LDAData { ldaAlphaTheta :: Double
                       , ldaAlphaPhi :: Double
                       , ldaNodes :: Set Node
                       , ldaItems :: Set Item
                       , ldaTopics :: Set Topic
                       , ldaNodeItems :: Seq (Node, Item)
                       }
               deriving (Show, Eq)

newtype Node = Node Int deriving (Show, Eq, Ord, Enum)
newtype Item = Item Int deriving (Show, Eq, Ord, Enum)
newtype Topic = Topic Int deriving (Show, Eq, Ord, Enum)
newtype NodeItem = NodeItem Int deriving (Show, Eq, Ord, Enum)

data LDAModel = LDAModel { mNodeItems :: EnumMap NodeItem (Node, Item)
                         , mThetas :: GatedPlate Node (DirMulti Topic)
                         , mPhis :: GatedPlate Topic (DirMulti Item)
                         , mTs :: GatedPlate NodeItem Topic
                         }

data ItemUnit = ItemUnit { iuNodes :: [Node]
                         , iuTopics :: [Topic]
                         , iuItems :: [Item]
                         , iuNodeItem :: NodeItem
                         , iuN :: Node
                         , iuT :: Shared Topic
                         , iuX :: Item
                         , iuTheta :: Shared (DirMulti Topic)
                         , iuPhis :: GatedPlate Topic (DirMulti Item)
                         }

model :: LDAData -> ModelMonad (Seq ItemUnit, LDAModel)
model d =
  do let LDAData {ldaTopics=topics, ldaNodes=nodes, ldaItems=items, ldaNodeItems=nodeItems} = d
         nis = EM.fromList $ zipWith (\idx (n,i)->(NodeItem idx, (n,i))) [0..] (toList nodeItems)
     thetas <- newGatedPlate (S.toList nodes) $ \n ->
       return $ symDirMulti (ldaAlphaTheta d) (S.toList topics)
     phis <- newGatedPlate (S.toList topics) $ \t ->
       return $ symDirMulti (ldaAlphaPhi d) (S.toList items)
     ts <- newGatedPlate (EM.keys nis) $ \ni ->
       liftRVar $ randomElementT $ SQ.fromList $ S.toList topics
  
     itemUnits <- forM (EM.toList ts) $ \(ni, t) ->
       do let (n,x) = nis EM.! ni
          let unit = ItemUnit { iuNodes = S.toList nodes
                              , iuTopics = S.toList topics
                              , iuItems = S.toList items
                              , iuNodeItem = ni
                              , iuN = n
                              , iuT = t
                              , iuX = x
                              , iuTheta = thetas EM.! n
                              , iuPhis = phis
                              }
          getShared t >>= guSet unit
          return unit
     let model = LDAModel { mNodeItems = nis
                          , mThetas = thetas
                          , mPhis = phis
                          , mTs = ts }
     return (SQ.fromList itemUnits, model)

likelihood :: LDAModel -> ModelMonad LogFloat
likelihood model =
  do a <- forM (EM.toList $ mNodeItems model) $ \(ni, (n,i)) ->
       do t <- getShared $ mTs model EM.! ni 
          theta <- getShared $ mThetas model EM.! n
          phi <- getShared $ mPhis model EM.! t
          return $ Product $ logFloat (prob theta t)
                           * logFloat (prob phi i)
     return $ getProduct $ mconcat a

instance GibbsUpdateUnit ItemUnit where
  type GUValue ItemUnit = Topic
  guProb unit t =
    do when (t `EM.notMember` iuPhis unit) $ liftIO $ putStrLn "unset"
       phi <- getShared $ iuPhis unit EM.! t 
       theta <- getShared $ iuTheta unit
       let th = probPretend theta t
       let ph = probPretend phi (iuX unit) 
       return $ th * ph
  
  guDomain = return . iuTopics
  
  guUnset unit =
    do t <- getShared $ iuT unit 
       let x = iuX unit
           theta = iuTheta unit
           phi = iuPhis unit EM.! t
       when (t `EM.notMember` iuPhis unit) $ liftIO $ putStrLn "unset"
       theta `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
  
  guSet unit t =
    do iuT unit `setShared` t
       let x = iuX unit
           theta = iuTheta unit
           phi = iuPhis unit EM.! t
       when (t `EM.notMember` iuPhis unit) $ liftIO $ putStrLn "unset"
       theta `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x

