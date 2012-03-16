{-# LANGUAGE TupleSections #-}

import BayesStack.Core
import BayesStack.Models.Topic.SharedTaste

import qualified Data.Map as M
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.EnumMap as EM
import Data.Foldable (forM_)
import Data.List
import Data.Function
import Data.Tuple (swap)
import Text.Printf
import Text.PrettyPrint
import Data.Random
import System.Random.MWC
import Control.Monad.IO.Class
import Data.Number.LogFloat hiding (realToFrac)

-- | Shared taste/LDA interpolation parameter
alphaGammaShared = 0.9

-- | Number of topics to train
topics = S.fromList $ [Topic i | i <- [1..3]]

-- | Number of Gibbs sweeps
nIter = 1000

vocabulary :: EM.EnumMap Item String
vocabulary = EM.fromList $ zip (map Item [1..])
             $ [ "cats"
               , "crafts"
               , "birds"
               , "dogs"
               , "christmas"
               , "Scotland"
               , "flowers"
               , "survival"
               , "dragons"
               , "vampires"
               , "angels"
               , "animals"
               , "autism"
               , "thriller"
               , "historical"
               , "horror"
               , "non-fiction"
               , "fantasy"
               , "mystery"
               ]

revVocabulary :: M.Map String Item
revVocabulary = M.fromList $ map swap $ EM.assocs vocabulary
               
-- | Input data to the model
stdata = STData { -- Content-enriched network
                  stNodes            = S.fromList $ map Node [1..5]
                , stFriendships      = S.fromList $ map Friendship $
                                       [ (Node 1, Node 2)
                                       , (Node 2, Node 4)
                                       , (Node 2, Node 3)
                                       , (Node 4, Node 5)
                                       , (Node 5, Node 1)
                                       ]
                , stItems            = S.fromList $ EM.keys vocabulary
                , stNodeItems        = setupNodeItems
                                       $ concatMap (\(n,items)->map (\i->(n, revVocabulary M.! i)) items)
                                       $ [ (Node 1, [ "cats", "crafts", "birds"
                                                    , "christmas", "flowers"
                                                    ]
                                           )
                                         , (Node 2, [ "cats", "birds", "animals"
                                                    , "angels", "dragons", "horror"
                                                    , "vampires"
                                                    ]
                                           )
                                         , (Node 3, [ "autism", "survival", "dragons"
                                                    , "vampires", "angels", "animals"
                                                    , "survival"
                                                    ]
                                           )
                                         , (Node 4, [ "thriller", "historical", "horror"
                                                    , "non-fiction", "vampires", "fantasy"
                                                    ]
                                           )
                                         , (Node 5, [ "vampires", "dragons", "angels"
                                                    , "horror", "fantasy", "thriller"
                                                    ]
                                           )
                                         ]
                
                -- Hyper-parameters and such
                , stAlphaGammaShared = alphaGammaShared
                , stAlphaGammaOwn    = 1 - alphaGammaShared
                , stAlphaPsi         = 0.1
                , stAlphaLambda      = 0.1
                , stAlphaPhi         = 0.1
                , stAlphaOmega       = 0.1
                , stTopics           = topics
                }

main :: IO ()
main = do
  state <- withSystemRandom $ runModel run
  let maybeInc (Just n) = Just $ n+1
      maybeInc Nothing  = Just 1
      wordCounts = foldl' (\a (n,x)->EM.alter maybeInc x a) EM.empty
                   $ EM.elems $ stNodeItems $ msData state
      totalCounts = EM.size $ stNodeItems $ msData state 
      
  liftIO $ putStr "\nTopics:\n"
  forM_ topics $ \t -> do
    let phi = msPhis state EM.! t
        probs = map (sampleProb phi) $ S.toList $ stItems $ msData state
    liftIO $ print $ text (show t) <+> colon
        <+> hsep ( punctuate comma
                 $ map (\(x,p)->text (vocabulary EM.! x) <> parens (text $ printf "%1.2e" p))
                 $ take 10 $ sortBy (flip (compare `on` snd))
                 $ zip (S.toList $ stItems $ msData state) probs
                 ) 
                 
  liftIO $ putStr "\nFriendship weights:\n"
  forM_ (stFriendships stdata) $ \(Friendship (a,b)) -> do
    let psi = msPsis state EM.! a
    liftIO $ putStr $ printf "%s\t%s\t\t%e\t%e\n"
             (show a) (show b)
             (friendInfluence state a b)
             (sampleProb psi b)

  liftIO $ putStr "\nShared topic mixtures:\n"
  forM_ (stFriendships stdata) $ \fs@(Friendship (a,b)) -> do
    let lambda = msLambdas state EM.! fs
    liftIO $ putStr $ printf "%s\t%s\t\t" (show a) (show b)
    liftIO $ putStr $ intercalate "\t"
                    $ map (\t->printf "%s(%e)\t" (show t) (sampleProb lambda t))
                    $ S.toList topics
    liftIO $ putStr "\n"
    
  liftIO $ putStr "\nOwn topic mixtures:\n"
  forM_ (stNodes stdata) $ \n -> do
    let omega = msOmegas state EM.! n
    liftIO $ putStr $ printf "%s\t\t" (show n)
    liftIO $ putStr $ intercalate "\t"
                    $ map (\t->printf "%s(%e)\t" (show t) (sampleProb omega t))
                    $ S.toList topics
    liftIO $ putStr "\n"

run :: ModelMonad STModelState
run = do
  initial <- liftRVar $ randomInitialize stdata
  (ius, model) <- model stdata initial
  
  state <- getModelState model
  liftIO $ putStrLn $ printf "Model log likelihood after initialization: %e"
                      (logFromLogFloat $ modelLikelihood state :: Double)
                      
  liftIO $ putStrLn $ printf "Created %d update units" (SQ.length ius)
  liftIO $ putStrLn "Starting inference..."
  forM_ [1..nIter::Int] $ \sweepN -> do
    concurrentFullGibbsUpdate 10 ius
    
  state <- getModelState model
  liftIO $ putStrLn $ printf "Model log likelihood after %d iterations: %e"
                      nIter (logFromLogFloat $ modelLikelihood state :: Double)
  return state
  
