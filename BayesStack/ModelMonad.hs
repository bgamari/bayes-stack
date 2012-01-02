module BayesStack.ModelMonad ( ModelMonad(..)
                             , liftRVar) where

import Data.Random.Internal.Source
import Data.RVar

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

-- | GibbsMonad?
newtype ModelMonad a = ModelMonad { runModel :: (RVarT IO a) }
instance Functor ModelMonad where
  fmap = liftM
  
instance Monad ModelMonad where
  return x = ModelMonad (return $! x)
  fail s = ModelMonad (fail s)
  (ModelMonad m) >>= k = ModelMonad (m >>= \x -> x `seq` runModel (k x))
  
instance Applicative ModelMonad where
  pure = return
  (<*>) = ap
  
instance MonadIO ModelMonad where
  liftIO = ModelMonad . liftIO
  
instance MonadRandom ModelMonad where
  getRandomPrim = ModelMonad . getRandomPrim
  
liftRVar :: RVarT IO a -> ModelMonad a
liftRVar = ModelMonad