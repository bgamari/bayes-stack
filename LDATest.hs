module Main where

import Data.EnumMap as EM
import System.Random.MWC
import Control.Monad.IO.Class
import BayesStack.Core
import LDA

priors = Priors { alphaTheta = 0.1
                , alphaPhi = 0.1
                }

nodes = [Node i | i <- [1..5]]
topics = [Topic i | i <- [1..5]]
items = [Item i | i <- [1..40]]
nodeItems = [ (Node 1, Item 1)
            ]

main = withSystemRandom $ runModel run
run = do (ius, thetas, phis) <- model priors nodes items topics nodeItems
         t <- getShared (thetas EM.! Node 1)
         liftIO $ print t

