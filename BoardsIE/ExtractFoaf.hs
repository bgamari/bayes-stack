{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveGeneric  #-}
import System.Directory
import Text.HTML.TagSoup
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import qualified System.FilePath.Find as FP
import System.FilePath.Find hiding (find)
 
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Data.Monoid
import qualified Data.ByteString as BS

import Data.Serialize
import Control.Concurrent.ParallelIO
import Types

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

getUsers :: IO [User]
getUsers = 
  do fs <- FP.find always (fileType ==? RegularFile) "/iesl/local/dietz/boardsie/foaf"
     liftM catMaybes $ parallelInterleaved $ map (runMaybeT . putUser) fs
  where putUser :: FilePath -> MaybeT IO User
        putUser f = do a <- tryReadFile f
                       liftIO $ putStrLn f
                       let user = parseFoaf a
                       guard (not $ S.null $ uKnows user)
                       return user

main =
  do users <- getUsers
     BS.writeFile "foaf.out" $ encode users
     stopGlobalPool

