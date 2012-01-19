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

-- | 'UniqueKey cat id' is a monad for a calculation of a mapping unique keys
-- 'id' on categorical values 'cat' to 
type UniqueKey cat id = UniqueKeyT cat id Identity
type UniqueKeyT cat id m = StateT (Int, Map cat id) m

newUniqueKey :: (Monad m, Ord cat) => (Int -> id) -> cat -> UniqueKeyT cat id m id
newUniqueKey f x =
  do (next, m) <- get
     if x `M.member` m then return $ m M.! x
                       else do put (next+1, M.insert x (f next) m)
                               return $ f next

runUniqueKey :: (Ord id) => UniqueKey cat id b -> b
runUniqueKey = runIdentity . runUniqueKeyT

runUniqueKeyT :: (Monad m, Ord id) => UniqueKeyT cat id m b -> m b
runUniqueKeyT a = evalStateT a (0, M.empty)

getInvMap :: (Monad m, Ord id) => UniqueKeyT cat id m (Map id cat)
getInvMap = do (_, m) <- get
               return $ M.fromList $ map swap $ M.toList m

runUniqueKeyTWithInvMap :: (Monad m, Ord id) => UniqueKeyT cat id m b -> m (b, Map id cat)
runUniqueKeyTWithInvMap a =
  runUniqueKeyT $ do result <- a
                     invMap <- getInvMap
                     return (result, invMap)

runUniqueKeyWithInvMap :: (Ord id) => UniqueKey cat id b -> (b, Map id cat)
runUniqueKeyWithInvMap a =
  runUniqueKey $ do result <- a
                    invMap <- getInvMap
                    return (result, invMap)

