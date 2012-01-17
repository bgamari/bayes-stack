module BayesStack.UniqueKey where

import Control.Monad.Trans.State
import Control.Monad (liftM)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Tuple

-- | 'UniqueKey cat id' is a monad for a calculation of a mapping unique keys
-- 'id' on categorical values 'cat' to 
type UniqueKeyT cat id m = StateT (Int, Map cat id) m

newUniqueKey :: (Monad m, Ord cat) => (Int -> id) -> cat -> UniqueKeyT cat id m id
newUniqueKey f x =
  do (next, m) <- get
     if x `M.member` m then return $ m M.! x
                       else do put (next+1, M.insert x (f next) m)
                               return $ f next

runUniqueKey :: (Monad m, Ord id) => UniqueKeyT cat id m b -> m b
runUniqueKey = liftM fst . runUniqueKeyWithInvMap

getInvMap :: (Monad m, Ord id) => UniqueKeyT cat id m (Map id cat)
getInvMap = do (_, m) <- get
               return $ M.fromList $ map swap $ M.toList m
runUniqueKeyWithInvMap :: (Monad m, Ord id) => UniqueKeyT cat id m b -> m (b, Map id cat)
runUniqueKeyWithInvMap a =
  evalStateT (do result <- a
                 invMap <- getInvMap
                 return (result, invMap)
             ) (0, M.empty)

