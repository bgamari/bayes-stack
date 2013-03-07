module Data.Random.Sequence (randomElementT) where

import Data.Random
import Data.Sequence as SQ

randomElementT :: Seq a -> RVarT m a
randomElementT xs | SQ.null xs = error "randomElementT: empty seq!"
randomElementT xs = do
  n <- uniformT 0 (SQ.length xs - 1)
  return (xs `index` n)
