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

import BayesStack.Models.SharedTaste
import BayesStack.UniqueKey
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
     
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads 

readFriendships :: IO [Friendship]
readFriendships = 
  do f' <- parseCSVFromFile "edges_with_profile.csv"
     let csv' = either (error . show) id f'
     return $ mapMaybe (\rec -> do when (length rec < 10) Nothing
                                   a <- maybeRead $ rec !! 8
                                   b <- maybeRead $ rec !! 9
                                   return $ Friendship (Node a, Node b))
                $ tail csv'

readTags :: IO ([(Node, Item)], Map Item String)
readTags =
  do f <- parseCSVFromFile "tags_for_users.csv"
     let csv = either (error . show) id f
         records = mapMaybe (\rec -> do a <- maybeRead $ rec !! 0
                                        return (a, map toLower $ rec !! 6))
                   $ tail csv
         (userTags, wordMap) = runUniqueKey $ forM records $ \(n,i) -> do k <- uniqueKey Item i
                                                                          return (Node n, k)

     return (userTags, wordMap)

newtype Group = Group Int deriving (Show, Eq, Enum)

readUserGroups :: IO [(Node, Group)]
readUserGroups =
  do csv <- liftM (either (error . show) id) $ parseCSVFromFile "r_usergroups_members.csv"
     return $ mapMaybe (\rec -> do u <- maybeRead $ rec !! 0
                                   g <- maybeRead $ rec !! 2
                                   return (Node u, Group g))
              $ tail csv

getGroups :: IO (EM.EnumMap Group [Node])
getGroups =
  do ugs <- readUserGroups
     return $ EM.fromListWith (++) $ map (\(n,g)->(g,[n])) ugs

