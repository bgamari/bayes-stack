module Main where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM
import Control.Monad

import BayesStack.Core
import BayesStack.ModelMonad
import BayesStack.Categorical
import BayesStack.DirMulti

main = runModel model

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
                         , iuN :: Categorical Node
                         , iuT :: Shared (Categorical Topic)
                         , iuX :: Categorical Item
                         , iuTheta :: Shared (DirMulti Topic)
                         , iuPhis :: Shared (GatedPlate Topic (DirMulti Item))
                         }

model :: Priors -> [Node] -> [(Node, Item)] -> [Topic] -> ModelMonad ()
model priors nodes nodeItems topics =
  do thetas <- EM.fromList $ forM nodes $ \n -> (n, dirMulti $ alphaTheta priors)
     phis <- EM.fromList $ forM topics $ \t -> (t, dirMulti $ alphaPhi priors)
  
     itemUnits <- forM nodeItems $ \(n,x) ->
       do n_ <- observedCategorical n
          t_ <- categorical
          x_ <- observedCategorical x
          return $ ItemUnit { iuN = n_
                            , iuT = t_
                            , iuX = x_
                            , iuTheta = thetas EM.! n
                            , iuPhis = phis
                            }
  
     forM_ itemUnits $ sample

instance Sampleable ItemUnit where
  type SValue = Topic
  sampleProb unit t =
    do th <- prob (iuTheta unit) t
       ph <- prob (iuPhis unit `gate` t) (iuX unit) 
       return $ th * ph
  
  range = iuTopics
  
  unset unit =
    do t <- getShared $ iuT unit 
       let x = iuX unit
           phi = iuPhis unit `gate` t
           theta = iuTheta unit
       theta `updateShared` decDirMulti t
       phi `updateShared` decDirMulti x
  
  set unit t =
    do iuT unit `setShared` t
       let theta = iuTheta unit
           phi = iuPhis unit `gate` t
           x = iuX unit
       theta `updateShared` incDirMulti t
       phi `updateShared` incDirMulti x
