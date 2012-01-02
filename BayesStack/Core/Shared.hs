module BayesStack.Core.Shared ( Shared, newShared
                              , randomShared
                              , getShared, setShared, updateShared
                              ) where

import Data.Random (sample)
import Data.Random.List

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import BayesStack.Core.ModelMonad

newtype Shared a = Shared (IORef a)

newShared :: a -> ModelMonad (Shared a)
newShared a = liftM Shared $ liftIO $ newIORef a

updateShared :: Shared a -> (a -> a) -> ModelMonad ()
updateShared (Shared a) f = liftIO $ atomicModifyIORef a (\x -> (f x, ()))
  
-- | Set a shared variable
infix 1 `setShared`
setShared :: Shared a -> a -> ModelMonad ()
(Shared a) `setShared` x = liftIO $ writeIORef a x
                           
getShared :: Shared a -> ModelMonad a
getShared (Shared a) = liftIO $ readIORef a

randomShared :: [a] -> ModelMonad (Shared a)
randomShared as = Data.Random.sample (randomElement as) >>= newShared
                     
