module BayesStack.Core.Shared (-- * Shared objects
                                Shared, newShared
                              , randomShared
                              , getShared, setShared, updateShared
                              -- * Maps of shared objects
                              , SharedEnumMap, newSharedEnumMap
                              , getSharedEnumMap
                              ) where

import Data.Random (sample)
import Data.Random.List

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import BayesStack.Core.ModelMonad

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

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


type SharedEnumMap control a = EnumMap control (Shared a)

newSharedEnumMap :: Enum control => [control] -> (control -> ModelMonad a) -> ModelMonad (SharedEnumMap control a)
newSharedEnumMap domain f =
  liftM EM.fromList $ forM domain $ \c -> do d <- f c >>= newShared
                                             return (c, d)

getSharedEnumMap :: Enum a => SharedEnumMap a b -> ModelMonad (EnumMap a b)
getSharedEnumMap = liftM EM.fromList . mapM (\(k,v)->do v' <- getShared v
                                                        return (k,v')
                                            ) . EM.toList

