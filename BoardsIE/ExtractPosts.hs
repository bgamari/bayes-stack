
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
import Database.MongoDB

tryReadFile :: MonadIO m => FilePath -> MaybeT m String
tryReadFile fp =
  MaybeT $ liftIO $ E.catch (TIO.readFile fp >>= return . Just . T.unpack)
                            (\e->do print (e :: IOError)
                                    return Nothing)
type UserId = String
type PostId = String

data Post = Post { pId :: PostId
                 , pUser :: UserId
                 }
            deriving (Show, Eq)

parsePost :: String -> Maybe Post
parsePost post =
  do let tags = parseTags post
     post <- listToMaybe $ dropWhile (~/= "<sioct:BoardPost>") tags
     maker <- listToMaybe $ dropWhile (~/= "<foaf:Person>") $ dropWhile (~/= "<foaf:maker>") tags
     return $ Post { pId = fromAttrib "rdf:about" post
                   , pUser = fromAttrib "rdf:about" maker
                   }

getPosts = 
  do fs <- liftIO $ FP.find always (fileType ==? RegularFile) "/iesl/canvas/dietz/boardsie/unzip/post"
     mapM_ (runMaybeT . putPost) fs
  where putPost :: FilePath -> MaybeT (Action IO) ()
        putPost f = do p <- tryReadFile f >>= maybe mzero return . parsePost
                       lift $ save "posts" [ "_id" =: u (pId p)
                                           , "user" =: u (pUser p)
                                           ]
                       liftIO $ putStrLn f


main =
  do pipe <- runIOE $ connect (Host "avon-2" (PortNumber 27025))
     access pipe master "boardsie" getPosts
     close pipe
