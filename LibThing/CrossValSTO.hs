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
     forM_ groups $ \g@(Group n) ->
       do f <- openFile (printf "group%d" n) WriteMode
          forM_ (stNodes $ msData state) $ \u ->
            do let isMember = (u,g) `elem` userGroups
               hPrintf f "%d" (if isMember then 1 else 0 :: Int)
               forM_ (zip [1..] $ S.toList $ stTopics $ msData state) $ \(i,t) ->
                 hPrintf f " %d:%f" (i::Int) (theta state u t)
               hPutStr f "\n"

