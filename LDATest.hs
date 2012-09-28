module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as SQ

import System.Random.MWC
import Data.Random
       
import Control.Monad.IO.Class
import Control.Monad (replicateM)
import Data.Foldable
import Control.Monad.Trans.State

import Text.PrettyPrint
import BayesStack.Core
import BayesStack.DirMulti
import BayesStack.Models.Topic.LDA

nodes = S.fromList [Node i | i <- [1..5]]
topics = S.fromList [Topic i | i <- [1..2]]
items = S.fromList [Item i | i <- [1..10]]
nodeItems = M.fromList $ zip (map NodeItem [0..])
            [ (Node 1, Item 1)
            , (Node 1, Item 1)
            , (Node 1, Item 1)
            , (Node 1, Item 2)
            , (Node 1, Item 2)
            , (Node 1, Item 2)

            , (Node 2, Item 1)
            , (Node 2, Item 1)
            , (Node 2, Item 1)
            , (Node 2, Item 1)
            , (Node 2, Item 1)
            , (Node 2, Item 2)
            , (Node 2, Item 2)
            , (Node 2, Item 2)
            , (Node 2, Item 2)
            , (Node 2, Item 2)

            , (Node 3, Item 1)
            , (Node 3, Item 1)
            , (Node 3, Item 1)
            , (Node 3, Item 1)
            , (Node 3, Item 1)
            , (Node 3, Item 2)
            , (Node 3, Item 2)
            , (Node 3, Item 2)
            , (Node 3, Item 2)
            , (Node 3, Item 2)
            , (Node 3, Item 3)
            , (Node 3, Item 3)
            , (Node 3, Item 3)
            , (Node 3, Item 3)
            , (Node 3, Item 3)

            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            , (Node 4, Item 3)
            ]

d = LDAData { ldaAlphaTheta = 0.1
            , ldaAlphaPhi = 0.1
            , ldaNodes = nodes
            , ldaItems = items
            , ldaTopics = topics
            , ldaNodeItems = nodeItems
            }

iter :: [WrappedUpdateUnit LDAState] -> StateT LDAState IO ()
iter uus = do
    ms <- get
    ms' <- liftIO $ gibbsUpdate ms uus
    put ms'
    liftIO $ showState ms'
    return ()

main = do
    init <- withSystemRandom $ \mwc -> runRVar (randomInitialize d) mwc :: IO ModelInit
    let state = model d init
        uus = updateUnits d
    ms <- execStateT (replicateM 100 $ iter uus) state
    return ()
    
showState :: LDAState -> IO ()
showState ms = do          
    putStrLn "\n\nThetas:"
    forM_ (take 5 $ S.toList nodes) $ \n->
        liftIO $ print $ text (show n) <+> prettyMultinom 5 show (stThetas ms M.! n)

    putStrLn "\nPhis:"
    forM_ (take 5 $ S.toList topics) $ \t->
        liftIO $ print $ text (show t) <+> prettyMultinom 3 show (stPhis ms M.! t)

