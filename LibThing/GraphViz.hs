{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Map (Map)
import qualified Data.EnumMap as EM
import qualified Data.Set as S
import System.Environment
import Control.Monad
import Data.Maybe
import Data.Serialize
import Data.Char (toLower)
import Text.CSV
import qualified Data.ByteString as BS
import Text.Dot 
import Text.Printf

import BayesStack.Models.Topic.SharedTasteSync
import BayesStack.Core
import LibThing.Data

enumNodeId :: Enum a => a -> NodeId
enumNodeId = userNodeId . fromEnum

main =
  do f:_ <- getArgs 
     s <- liftM decode $ BS.readFile f :: IO (Either String STModelState)
     case s of
        Left e -> putStrLn e
        otherwise -> return ()
     let Right state = s

     (userTags, wordMap) <- readTags
     let fs = S.toList $ stFriendships $ msData state

     groups <- getGroups
     --print $ map (prob (msLambdas state EM.! head fs)) $ S.toList $ stTopics $ msData state

     let topics = S.toList $ stTopics $ msData state
         nTopics = realToFrac $ length topics

     let d :: Topic -> Dot ()
         d t = do attribute ("width", "1")
                  attribute ("height", "1")
                  attribute ("splines", "false")
                  attribute ("overlap", "prism")
                  forM_ userTags $ \(n,_) -> userNode (enumNodeId n) [ ("style", "filled")
                                                                     , ("label", "")
                                                                     , ("fillcolor", "white")
                                                                     ]
                  let friendshipWeight (Friendship (a,b)) =
                          prob (msPsis state EM.! a) b
                        * realToFrac (length $ filter (isFriend a) fs)
                        * prob (msPsis state EM.! b) a
                        * realToFrac (length $ filter (isFriend b) fs)
                  let topicWeight f = prob (msLambdas state EM.! f) t
                  let sharedGroups (Friendship (a,b)) =
                       length $ filter (\nodes->a `elem` nodes && b `elem` nodes) $ EM.elems groups

                  let --penWidth f = 3 * friendshipWeight f
                      --penWidth f = 1 + 3*sharedGroups f
                      penWidth f = 5
                      color :: Friendship -> (Double,Double,Double)
                      color f = ( if sharedGroups f > 0 then 0 else 0.5
                                , 0.8
                                , 0.5) --if topicWeight f > 1/3 && friendshipWeight f > 0.5 then 0 else 1)

                  forM_ fs $ \f@(Friendship (a,b)) ->
                    do when (not $ topicWeight f > 1/nTopics && friendshipWeight f > 0.2) $
                          edge (enumNodeId a) (enumNodeId b) [ ("color", case color f of (h,s,v) -> printf "%1.3f %1.3f %1.3f" h (0.1::Double) (0.8::Double))
                                                             , ("penwidth", "1")
                                                             , ("len", "2")
                                                             , ("dirType", "none")
                                                             ]
                       when (topicWeight f > 1/nTopics && friendshipWeight f > 0.2) $
                         do userNode (enumNodeId a) [ ("fillcolor", "black") ]
                            userNode (enumNodeId b) [ ("fillcolor", "black") ]
                            edge (enumNodeId a) (enumNodeId b) [ ("color", case color f of (h,s,v) -> printf "%1.3f %1.3f %1.3f" h s v)
                                                               , ("penwidth", show $ penWidth f)
                                                               , ("len", "0.5")
                                                               , ("dirType", "none")
                                                               ]
     forM_ topics $ \t->
       writeFile (show t++".dot") $ showDot $ d t
     
