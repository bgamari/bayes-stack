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
import BayesStack.Models.Topic.SharedTasteOwn
import LibThing.Data

import Text.CSV
import Text.PrettyPrint
import Text.Printf

theta :: STModelState -> Node -> Topic -> Probability
theta state u t =
  prob gamma True * prob omega t
  + sum (map (\f->let lambda = msLambdas state EM.! Friendship (u,f)
                  in prob psi f * prob lambda t * prob gamma False
             ) $ getFriends (S.toList $ stFriendships $ msData state) u
        )
  where psi = msPsis state EM.! u
        gamma = msGammas state EM.! u
        omega = msOmegas state EM.! u

main =
  do f:_ <- getArgs
     s <- liftM decode $ BS.readFile f
     case s of
        Left e -> putStrLn e
        otherwise -> return ()
     let Right state = s
     userGroups <- readUserGroups
     let groups = nub $ map snd userGroups

     createDirectoryIfMissing False $ f++"-crossval-nodes"
     createDirectoryIfMissing False $ f++"-crossval-edges"
     forM_ groups $ \g@(Group n) ->
       do f <- openFile (printf "%s-crossval-nodes/group%d" f n) WriteMode
          forM_ (stNodes $ msData state) $ \u ->
            do let isMember = (u,g) `elem` userGroups
               hPrintf f "%d" (if isMember then 1 else -1 :: Int)
               forM_ (zip [1..] $ S.toList $ stTopics $ msData state) $ \(i,t) ->
                 hPrintf f " %d:%f" (i::Int) (theta state u t)
               hPutStr f "\n"

     groups <- getGroups
     forM_ (EM.toList groups) $ \(Group i, members) ->
       do f <- openFile (printf "%s-crossval-edges/group%d" f i) WriteMode
          forM_ (stFriendships $ msData state) $ \(Friendship (a,b)) ->
            do let isMember = a `elem` members && b `elem` members
               hPrintf f "%d" (if isMember then 1 else -1 :: Int)
               forM_ (zip [1..] $ S.toList $ stTopics $ msData state) $ \(j,t) ->
                 let lambda = msLambdas state EM.! Friendship (a,b)
                 in hPrintf f " %d:%f" (j::Int) (prob lambda t)
               hPutStr f "\n"

