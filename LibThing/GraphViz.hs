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

import BayesStack.Models.Topic.SharedTaste
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
     let fs = S.toList $ msFriendships state

     groups <- getGroups

     let t = head $ S.toList $ msTopics state
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
                        * realToFrac (S.size $ S.filter (isFriend a) (msFriendships state))
                        * prob (msPsis state EM.! b) a
                        * realToFrac (S.size $ S.filter (isFriend b) (msFriendships state))
                  let topicWeight f = prob (msLambdas state EM.! f) t
                  let sharedGroups (Friendship (a,b)) =
                       length $ filter (\nodes->a `elem` nodes && b `elem` nodes) $ EM.elems groups

                  let --penWidth f = 30 * friendshipWeight f
                      --penWidth f = 1 + 3*sharedGroups f
                      penWidth f = 3
                      color :: Friendship -> (Double,Double,Double)
                      color f = (if sharedGroups f > 0 then 0.5 else 0, 0.5, 0.8-0.6*topicWeight f)

                  forM_ fs $ \f@(Friendship (a,b)) -> edge (enumNodeId a) (enumNodeId b)
                                                          [ ("color", case color f of (r,g,b) -> printf "%1.3f %1.3f %1.3f" r g b)
                                                          , ("penwidth", show $ penWidth f)
                                                          , ("len", "1")
                                                          , ("dirType", "none")
                                                          ]
     forM_ (S.toList $ msTopics state) $ \t->
       writeFile (show t++".dot") $ showDot $ d t
     
