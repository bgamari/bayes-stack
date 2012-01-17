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

import Debug.Trace
tryReadFile :: MonadIO m => FilePath -> MaybeT m String
tryReadFile fp =
  MaybeT $ liftIO $ E.catch (TIO.readFile fp >>= return . Just . T.unpack)
                            (\e->do print (e :: IOError)
                                    return Nothing)
type UserId = String

data User = User { uId :: UserId
                 , uNick :: String
                 , uKnows :: Set UserId }
            deriving (Show, Eq)

-- * FOAF
parseFoaf :: String -> User
parseFoaf foaf =
  let tags = parseTags foaf
  in User { uId = fromAttrib "rdf:about" $ head
                  $ dropWhile (~/= "<foaf:Person>") tags
          , uNick = fromTagText $ head
                    $ tail $ dropWhile (~/= "<foaf:nick>") tags
          , uKnows = S.fromList
                     $ map (fromAttrib "rdf:about" . head . dropWhile (~/= "<foaf:Person>"))
                     $ sections (~== "<foaf:knows>") tags
          }

getUsers :: Action IO ()
getUsers = 
  do fs <- liftIO $ FP.find always (fileType ==? RegularFile) "/iesl/canvas/dietz/boardsie/unzip/foaf"
     mapM_ (runMaybeT . putUser) fs
  where putUser :: FilePath -> MaybeT (Action IO) ()
        putUser f = do a <- tryReadFile f >>= return . parseFoaf
                       guard (not $ S.null $ uKnows a)
                       lift $ save "users" [ "_id" =: u (uId a)
                                           , "nick" =: u (uNick a)
                                           , "knows" =: map u (S.toList $ uKnows a)
                                           ]
                       liftIO $ putStrLn f

main =
  do pipe <- runIOE $ connect (Host "avon-2" (PortNumber 27025))
     putStrLn "Users"
     access pipe master "boardsie" getUsers
     close pipe

