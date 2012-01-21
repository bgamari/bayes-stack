{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (any, sum)
import Data.Map (Map)
import qualified Data.EnumMap as EM
import qualified Data.Set as S
import System.Environment
import Control.Monad hiding (forM_)
import Data.Maybe
import Data.Serialize
import Data.Foldable hiding (elem)
import Data.Char (toLower)
import Text.CSV
import qualified Data.ByteString as BS
import Text.Dot 
import Text.Printf

import BayesStack.Models.Topic.SharedTasteOwnSync
import BayesStack.Core

enumNodeId :: Enum a => a -> NodeId
enumNodeId = userNodeId . fromEnum

main =
  do f:_ <- getArgs 
     s <- liftM decode $ BS.readFile f :: IO (Either String STModelState)
     case s of
        Left e -> putStrLn e
        otherwise -> return ()
     let Right state = s

     let fs = S.toList $ stFriendships $ msData state
         topics = S.toList $ stTopics $ msData state
         nTopics = realToFrac $ length topics
         nodes = S.toList $ stNodes $ msData state
         friends = foldMap (\n->EM.singleton n $ S.fromList $ getFriends fs n) nodes
         psiWeight :: Node -> Node -> Double
         psiWeight u f =
           let lambda = msLambdas state EM.! Friendship(u,f)
               -- ivT -- for all nodeitems, the current configuration for t
               tProbF = map (log . prob lambda . ivT) $ EM.elems $ msVars state -- prob of t under lambda_{u,f}
               tMean = exp $ (sum tProbF)/(realToFrac $ EM.size $ msVars state) -- the geometric mean of tProbF
           in tMean
         psiCache = foldMap (\(a,b)->EM.singleton (a,b) $ psiWeight a b) $
                        map (\(Friendship (u,f))->(u,f)) fs ++ map (\(Friendship (u,f))->(f,u)) fs

     let d :: Topic -> Dot ()
         d t = do attribute ("width", "1")
                  attribute ("height", "1")
                  attribute ("splines", "false")
                  attribute ("overlap", "prism")
                  let friendshipWeight (Friendship (a,b)) = psiCache EM.! (a,b)

                  let topicWeight f = prob (msLambdas state EM.! f) t

                  let includeFriendship :: Friendship -> Bool
                      includeFriendship fs@(Friendship (u,f)) =
                        topicWeight fs > 1/nTopics && psiCache EM.! (u,f) > 0.000 && psiCache EM.! (f,u) > 0.000
                  let includeUser :: Node -> Bool
                      includeUser u = 
                        any (\f -> includeFriendship $ Friendship (u,f)) $ friends EM.! u

                  forM_ nodes $ \n ->
                    when (includeUser n) $
                      userNode (enumNodeId n) [ ("style", "filled")
                                              , ("label", "")
                                              , ("height", "1")
                                              , ("width","1")
                                              , ("fontsize","25.0")
                                              , ("fillcolor", "white")
                                              ]

                  
                  let penWidth fs@(Friendship (u,f)) = 500 * (sqrt $ psiCache EM.! (u, f) * psiCache EM.! (f, u))
                      color :: Friendship -> (Double,Double,Double)
                      color f = ( 0.5
                                , 0.8
                                , 0.5)

                  forM_ fs $ \f@(Friendship (a,b)) ->
                    do when (includeFriendship f) $
                            edge (enumNodeId a) (enumNodeId b) [ ("color", "grey50")
                                                               , ("penwidth", show $ penWidth f)
                                                               , ("weight","5.0")
                                                               , ("dir", "none")
                                                               ]

     forM_ topics $ \t@(Topic n)->
       writeFile (printf "topic%03d.dot" n) $ showDot $ d t
     
