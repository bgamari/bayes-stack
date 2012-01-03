module Main where

import Data.EnumMap as EM
import System.Random.MWC
import Control.Monad
import Control.Monad.IO.Class

import Text.PrettyPrint
import BayesStack.Core
import BayesStack.DirMulti
import LDA

priors = Priors { alphaTheta = 0.1
                , alphaPhi = 0.1
                }

nodes = [Node i | i <- [1..5]]
topics = [Topic i | i <- [1..5]]
items = [Item i | i <- [1..10]]
nodeItems = [ (Node 1, Item 1)
            , (Node 2, Item 1)
            , (Node 3, Item 2)
            , (Node 4, Item 2)
            , (Node 5, Item 2)
            , (Node 5, Item 1)
            ]

main = withSystemRandom $ runModel run
run = do (ius, thetas, phis) <- model priors nodes items topics nodeItems
         let iterate = do forM_ ius gibbsUpdate
                          forM_ (take 5 nodes) $ \n ->
                                do th <- getShared (thetas EM.! n)
                                   liftIO $ print $ text (show n) <+> prettyDirMulti 5 th
         replicateM 100 iterate

