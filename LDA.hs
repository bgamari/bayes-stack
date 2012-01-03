{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module LDA where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
import Control.Monad
import Data.Random.List (randomElement)
import qualified Data.Random (sample)

import BayesStack.Core
import BayesStack.Categorical
import BayesStack.DirMulti


data Priors = Priors { alphaTheta :: Double
                     , alphaPhi :: Double
                     }
              deriving (Show, Eq)

newtype Node = Node Int deriving (Show, Eq, Ord, Enum)
newtype Item = Item Int deriving (Show, Eq, Ord, Enum)
newtype Topic = Topic Int deriving (Show, Eq, Ord, Enum)

data ItemUnit = ItemUnit { iuNodes :: [Node]
                         , iuTopics :: [Topic]
                         , iuItems :: [Item]
                         , iuN :: Node
                         , iuT :: Shared Topic
                         , iuX :: Item
                         , iuTheta :: Shared (DirMulti Topic)
                         , iuPhis :: GatedPlate Topic (DirMulti Item)
                         }

model :: Priors -> [Node] -> [Item] -> [Topic] -> [(Node, Item)] -> ModelMonad ([ItemUnit], GatedPlate Node (DirMulti Topic), GatedPlate Topic (DirMulti Item))
model priors nodes items topics nodeItems =
  do thetas <- liftM EM.fromList $ forM nodes
        $ \n -> do theta <- newShared $ symDirMulti (alphaTheta priors) topics
                   return (n, theta)
     phis <- liftM EM.fromList $ forM topics
        $ \t -> do phi <- newShared $ symDirMulti (alphaPhi priors) items
                   return (t, phi)
     initTs <- forM nodeItems $ const $ Data.Random.sample $ randomElement topics
  
     itemUnits <- forM (zip nodeItems initTs) $ \((n,x), t) ->
       do t_ <- newShared t
          let unit = ItemUnit { iuNodes = nodes
                              , iuTopics = topics
                              , iuItems = items
                              , iuN = n
                              , iuT = t_
                              , iuX = x
                              , iuTheta = thetas EM.! n
                              , iuPhis = phis
                              }
          set unit t
          return unit
     return (itemUnits, thetas, phis)

instance Sampleable ItemUnit where
  type SValue ItemUnit = Topic
  sampleProb unit t =
    do phi <- getShared $ iuPhis unit EM.! t 
       theta <- getShared $ iuTheta unit
       let th = prob theta t
       let ph = prob phi (iuX unit) 
       return $ th * ph
  
  range = return . iuTopics
  
  unset unit =
    do t <- getShared $ iuT unit 
       let x = iuX unit
           theta = iuTheta unit
           phi = iuPhis unit EM.! t
       theta `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
  
  set unit t =
    do iuT unit `setShared` t
       let x = iuX unit
           theta = iuTheta unit
           phi = iuPhis unit EM.! t
       theta `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x

