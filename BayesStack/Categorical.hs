module BayesStack.Categorical (categorical, Categorical, getCategorical) where

import BayesStack.Core

import Data.Random
 
data Categorical a = Categorical (Shared a)

categorical :: Enum a => [a] -> ModelMonad (Categorical a)
categorical range = 
  do a <- Data.Random.sample $ randomElement range
     s <- newShared a
     return $ Categorical s

getCategorical :: Categorical a -> ModelMonad a
getCategorical (Categorical s) = getShared s

