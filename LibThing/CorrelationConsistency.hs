import Control.Monad (liftM)
import Control.Monad.Trans.State
import Data.Function (on)

import Data.Traversable
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.EnumMap as EM

import Data.Serialize
import BayesStack.Core
import BayesStack.DirMulti
import System.Environment
import System.IO

import LibThing.Data
import BayesStack.Models.Topic.Types
import BayesStack.Models.Topic.SharedTasteOwnSync

import Text.CSV
import Text.PrettyPrint
import Text.Printf

theta :: STModelState -> Node -> Topic -> Probability
theta state u t =
  sum $ map (\f->let lambda = msLambdas state EM.! Friendship (u,f)
                 in prob psi f * prob lambda t
            ) $ getFriends (S.toList $ stFriendships $ msData state) u
  where psi = msPsis state EM.! u


getUserGroups :: STModelState -> IO (EM.EnumMap Node (Set Group), [Group])
getUserGroups state =
  do records <- readUserGroups
     let a = EM.fromListWith S.union $ map (\(n,g)->(n, S.singleton g)) records
     return (a, nub $ map snd records)

pearsonCorr :: [(Double, Double)] -> Double
pearsonCorr s =
  let n = realToFrac $ length s
      mean xs = sum xs / n
      std xs = sqrt (sum $ map (\x->(x-mean xs)^2 / n) xs)
      (xs,ys) = unzip s
  in 1/(n-1) * sum (map (\(x,y)->(x-mean xs)/std xs * (y-mean ys)/std ys) s)

topicGroupCorr :: STModelState -> EM.EnumMap Node (Set Group) -> Topic -> Group -> Double
topicGroupCorr state userGroups topic group =
    pearsonCorr $ map (\u->(theta state u topic, weight u))
  $ S.toList $ stNodes $ msData state
  where weight u | group `S.member` (userGroups EM.! u) =
                                1 / realToFrac (S.size $ userGroups EM.! u)
                 | otherwise = 0

edgeGroupCorr :: STModelState -> EM.EnumMap Node (Set Group) -> Topic -> Group -> Double
edgeGroupCorr state userGroups topic group =
    pearsonCorr
  $ mapMaybe (\fs->do w <- weight fs
                      return (prob (msLambdas state EM.! fs) topic, w))
  $ S.toList $ stFriendships $ msData state
  where weight (Friendship (a,b))
          | group `S.member` (userGroups EM.! a) && group `S.member` (userGroups EM.! b)  =
                Just $ 1 / realToFrac (S.size $ S.intersection (userGroups EM.! a) (userGroups EM.! b))
          | otherwise = Nothing

mean xs = sum xs / realToFrac (length xs)

auc :: Ord a => [a] -> Set a -> Double
auc predRanking gndTruth =
  let pairs (x:rest) = map (\y->(x,y)) rest ++ pairs rest
      predPairs = map (\(a,b)->( a `S.member` gndTruth
                               , b `S.member` gndTruth ))
                  $ pairs predRanking
      num = length $ filter (\(a,b)->a && not b) predPairs
      denom = length $ filter (\(a,b)->a `xor` b) predPairs
  in realToFrac num / realToFrac denom

xor True False = True
xor False True = True
xor _ _ = False

main =
  do f:_ <- getArgs
     Right state <- liftM decode $ BS.readFile f

     (userGroups, groups) <- getUserGroups state
     let topics = S.toList $ stTopics $ msData state
         nodes = S.toList $ stNodes $ msData state

     let groupWeight g u = sum $ map (\t->theta state u t * topicGroupCorr state userGroups t g) topics
         nodeAUC u =
           let trueGroups = userGroups EM.! u
               groupOrder = sortBy (\g1 g2->groupWeight g2 u `compare` groupWeight g1 u) groups
           in case () of
                _ | trueGroups == S.fromList groups  -> Nothing
                _ | S.null trueGroups     -> Nothing
                _ | otherwise             -> Just $ auc groupOrder trueGroups

     putStr "Nodes: "
     print $ mean $ mapMaybe nodeAUC nodes

     let groupWeight g fs = let lambda = msLambdas state EM.! fs
                            in sum $ map (\t->prob lambda t * edgeGroupCorr state userGroups t g) topics
         edgeAUC fs@(Friendship (u,f)) =
           let trueGroups = (userGroups EM.! u) `S.intersection` (userGroups EM.! f)
               groupOrder = sortBy (\g1 g2->groupWeight g2 fs `compare` groupWeight g1 fs) groups
           in case () of
                _ | trueGroups == S.fromList groups  -> Nothing
                _ | S.null trueGroups     -> Nothing
                _ | otherwise             -> Just $ auc groupOrder trueGroups
     
     putStr "Edges: "
     print $ mean $ mapMaybe edgeAUC $ S.toList $ stFriendships $ msData state

