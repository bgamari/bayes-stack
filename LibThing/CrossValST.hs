import Control.Monad (liftM)
import Control.Monad.Trans.State
import Data.Function (on)

import Data.Traversable
import Data.Foldable hiding (elem, sum)
import Data.List (nub)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.EnumMap as EM

import System.IO
import System.Directory
import System.Environment
import Data.Serialize

import BayesStack.Core
import BayesStack.DirMulti
import BayesStack.Models.Topic.SharedTasteSync
import LibThing.Data

import Text.CSV
import Text.PrettyPrint
import Text.Printf

theta :: STModelState -> Node -> Topic -> Probability
theta state u t =
  sum $ map (\f->let lambda = msLambdas state EM.! Friendship (u,f)
                 in prob psi f * prob lambda t
            ) $ getFriends (S.toList $ stFriendships $ msData state) u
  where psi = msPsis state EM.! u

main =
  do f:_ <- getArgs
     s <- liftM decode $ BS.readFile f
     case s of
        Left e -> putStrLn e
        otherwise -> return ()
     let Right state = s

     groups <- getGroups
     createDirectoryIfMissing False $ f++"-crossval-nodes"
     createDirectoryIfMissing False $ f++"-crossval-edges"
     forM_ (EM.toList groups) $ \(Group i, members) ->
       do f <- openFile (printf "%s-crossval-nodes/group%d" f i) WriteMode
          forM_ (stNodes $ msData state) $ \u ->
            do let isMember = u `elem` members
               hPrintf f "%d" (if isMember then 1 else -1 :: Int)
               forM_ (zip [1..] $ S.toList $ stTopics $ msData state) $ \(j,t) ->
                 hPrintf f " %d:%f" (j::Int) (theta state u t)
               hPutStr f "\n"

     forM_ (EM.toList groups) $ \(Group i, members) ->
       do f <- openFile (printf "%s-crossval-edges/group%d" f i) WriteMode
          forM_ (stFriendships $ msData state) $ \(Friendship (a,b)) ->
            do let isMember = a `elem` members && b `elem` members
               hPrintf f "%d" (if isMember then 1 else -1 :: Int)
               forM_ (zip [1..] $ S.toList $ stTopics $ msData state) $ \(j,t) ->
                 let lambda = msLambdas state EM.! Friendship (a,b)
                 in hPrintf f " %d:%f" (j::Int) (prob lambda t)
               hPutStr f "\n"

