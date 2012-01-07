module Main where

import qualified Data.EnumMap as EM
import qualified Data.Set as S
import qualified Data.Sequence as SQ

import System.Random.MWC
import Control.Monad.IO.Class
import Control.Monad (replicateM)
import Data.Foldable

import Text.PrettyPrint
import BayesStack.Core
import BayesStack.DirMulti
import LDA

nodes = S.fromList [Node i | i <- [1..5]]
topics = S.fromList [Topic i | i <- [1..5]]
items = S.fromList [Item i | i <- [1..10]]
nodeItems = SQ.fromList
            [ (Node 1, Item 1)
            , (Node 2, Item 1)
            , (Node 3, Item 2)
            , (Node 4, Item 2)
            , (Node 5, Item 2)
            , (Node 5, Item 1)
            ]

d = LDAData { ldaAlphaTheta = 0.1
            , ldaAlphaPhi = 0.1
            , ldaNodes = nodes
            , ldaItems = items
            , ldaTopics = topics
            , ldaNodeItems = nodeItems
            }

main = withSystemRandom $ runModel run
run = do (ius, thetas, phis) <- model d
         let iterate = do forM_ ius gibbsUpdate
                          forM_ (take 5 $ S.toList nodes) $ \n ->
                                do th <- getShared (thetas EM.! n)
                                   liftIO $ print $ text (show n) <+> prettyDirMulti 5 th
         replicateM 100 iterate

