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
import BayesStack.Models.Topic.LDA
import LibThing.Data

import Text.CSV
import Text.PrettyPrint
import Text.Printf

main =
  do f:_ <- getArgs
     s <- liftM decode $ BS.readFile f
     case s of
        Left e -> putStrLn e
        otherwise -> return ()
     let Right state = s

     userGroups <- readUserGroups
     let groups = nub $ map snd userGroups
         nodes = ldaNodes $ msData state
         topics = ldaTopics $ msData state
     createDirectoryIfMissing False $ f++"-crossval-nodes"
     forM_ groups $ \g@(Group n) ->
       do f <- openFile (printf "%s-crossval-nodes/group%d" f n) WriteMode
          forM_ nodes $ \u ->
            do let isMember = (u,g) `elem` userGroups
               hPrintf f "%d" (if isMember then 1 else -1 :: Int)
               forM_ (zip [1..] $ S.toList topics) $ \(i,t) ->
                 hPrintf f " %d:%f" (i::Int) (prob (msThetas state EM.! u) t)
               hPutStr f "\n"


