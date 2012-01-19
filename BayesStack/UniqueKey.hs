module BayesStack.UniqueKey where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad (liftM)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Tuple
import Data.Functor.Identity

data UKState cat id = UKState { ukNextIdx :: Int
                              , ukFromCat :: Map cat id
                              }

type CatMap id = Int -> id

-- | 'UniqueKey cat id' is a monad for a calculation to assign unique
-- keys 'id' (e.g. an internal integer representation wrapped in a newtype) to
-- categorical values 'cat'
type UniqueKey cat id = UniqueKeyT cat id Identity
newtype UniqueKeyT cat id m a = UniqueKeyT { unUniqueKeyT :: (StateT (UKState cat id) (ReaderT (CatMap id) m) a) }

instance Monad m => Monad (UniqueKeyT cat id m) where
  -- UniqueKey a -> (a -> UniqueKey b) -> UniqueKey b
  (UniqueKeyT m) >>= f  =  UniqueKeyT (m >>= \x -> unUniqueKeyT (f x))
  return = UniqueKeyT . return

instance MonadTrans (UniqueKeyT cat id) where
  lift = UniqueKeyT . lift . lift

instance MonadIO m => MonadIO (UniqueKeyT cat id m) where
  liftIO = lift . liftIO

newUniqueKey :: (Monad m, Ord cat) => cat -> UniqueKeyT cat id m id
newUniqueKey x = UniqueKeyT $
  do UKState {ukNextIdx=next, ukFromCat=from} <- get
     catmap <- lift $ ask
     if x `M.member` from
       then return $ from M.! x
       else do let c = catmap next
               put $ UKState { ukNextIdx = next+1
                             , ukFromCat = M.insert x c from
                             }
               return c

runUniqueKey :: (Ord id) => CatMap id -> UniqueKey cat id b -> b
runUniqueKey catmap m = runIdentity $ runUniqueKeyT catmap m

runUniqueKeyT :: (Monad m, Ord id) => CatMap id -> UniqueKeyT cat id m b -> m b
runUniqueKeyT catmap (UniqueKeyT a) = runReaderT (evalStateT a initial) catmap
  where initial = UKState { ukNextIdx = 0, ukFromCat = M.empty }

getMapToCat :: (Monad m, Ord id) => UniqueKeyT cat id m (Map id cat)
getMapToCat = UniqueKeyT $ liftM (M.fromList . map swap . M.toList . ukFromCat) $ get

getMapFromCat :: (Monad m, Ord id) => UniqueKeyT cat id m (Map cat id)
getMapFromCat = UniqueKeyT $ liftM ukFromCat get

runUniqueKeyTWithInvMap :: (Monad m, Ord id) => CatMap id -> UniqueKeyT cat id m b -> m (b, Map id cat)
runUniqueKeyTWithInvMap catmap a =
  runUniqueKeyT catmap $ do result <- a
                            invMap <- getMapToCat
                            return (result, invMap)

runUniqueKeyWithInvMap :: (Ord id) => CatMap id -> UniqueKey cat id b -> (b, Map id cat)
runUniqueKeyWithInvMap catmap a =
  runUniqueKey catmap $ do result <- a
                           invMap <- getMapToCat
                           return (result, invMap)

