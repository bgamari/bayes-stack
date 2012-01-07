module Data.Sequence.Chunk (chunk) where

import Data.Sequence as SQ

-- | 'chunk n xs' splits 'xs' into 'n' chunks
chunkSeq :: Int -> Seq a -> Seq (Seq a)
chunkSeq n xs = let m = ceiling $ realToFrac (SQ.length xs) / realToFrac n
                    f xs | SQ.null xs = SQ.empty
                    f xs = SQ.take m xs : (f $ SQ.drop m xs)
                in f xs

