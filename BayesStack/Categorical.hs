module BayesStack.Categorical(observedCategorical, categorical, Categorical) where

import BayesStack.Core
import BayesStack.ModelMonad

import Data.Random
import qualified Data.Random.Distribution.Categorical as C
 
data Categorical a = Categorical (Shared a)

observedCategorical :: Enum a => a -> ModelMonad (Categorical a)
observedCategorical a =
  do b <- newShared a
     return $ Categorical b
    
categorical :: Enum a => [a] -> ModelMonad (Categorical a)
categorical range =
  do a <- Data.Random.sample $ randomElement range
     observedCategorical a 
