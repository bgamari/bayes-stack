
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import System.Directory
import Text.HTML.TagSoup
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import qualified System.FilePath.Find as FP
import System.FilePath.Find hiding (find)
import qualified Control.Exception as E
 
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Concurrent.ParallelIO
import Data.Serialize
import Types

parseThread :: String -> Maybe Thread
parseThread thread = 
  do let tags = parseTags thread
     thread <- listToMaybe $ dropWhile (~/= "<sioc:Thread>") tags
     title <- listToMaybe $ dropWhile (not . isTagText)  $ dropWhile (~/= "<dc:title>") $ dropWhile (~/= "<sioc:Thread>") tags
     forum <- listToMaybe $ dropWhile (~/= "<sioc:Forum>") tags
     let posts = map (fromAttrib "rdf:about") $ filter (isTagOpenName "sioc:Post") tags
     return $ Thread { tId = fromAttrib "rdf:about" thread
                     , tTitle = fromTagText title
                     , tForum = fromAttrib "rdf:about" forum
                     , tPosts = S.fromList posts
                     }

getForums :: IO [Forum]
getForums = 
  do fs <- liftIO $ FP.find always (fileType ==? RegularFile) rootPath
     mapM_ (runMaybeT . putThread) fs
  where putForum :: FilePath -> MaybeT (Action IO) ()
        putForum f = do t <- tryReadFile f >>= maybe mzero return . parseThread
                        lift $ save "threads" [ "_id" =: u (tId t)
                                              , "title" =: u (tTitle t)
                                              , "forum" =: u (tForum t)
                                              ]
                        lift $ modify (Select ["_id" =: u (tId t)] "threads")
                                      [ "$pushAll" =: ["posts" =: map u (S.toList $ tPosts t) ] ]
                        liftIO $ putStrLn f
                        return ()

main =
  do forums <- getForums
     BS.writeFile "forums.out" $ encode users
     stopGlobalPool

