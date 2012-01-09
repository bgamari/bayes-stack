module BayesStack.UniqueKey where

import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Map as M

import Data.Tuple

-- | 'UniqueKey cat id' is a monad for a calculation of a mapping unique keys
-- 'id' on categorical values 'cat' to 
type UniqueKey cat id = State (Int, Map cat id)

uniqueKey :: Ord cat => (Int -> id) -> cat -> UniqueKey cat id id
uniqueKey f x =
  do (next, m) <- get
     if x `M.member` m then return $ m M.! x
                       else do put (next+1, M.insert x (f next) m)
                               return $ f next

runUniqueKey :: Ord id => UniqueKey cat id b -> (b, Map id cat)
runUniqueKey a =
  evalState (do result <- a
                (_, m) <- get
                let invMap = M.fromList $ map swap $ M.toList m
                return (result, invMap)
            ) (0, M.empty)

