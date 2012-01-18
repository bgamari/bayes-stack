{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
 
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Text (Text)

import Text.HTML.TagSoup
import Database.Redis
import Types
import Extract

parsePost :: Text -> Maybe Post
parsePost post =
  do let tags = parseTags post
     post <- listToMaybe $ dropWhile (~/= "<sioct:BoardPost>") tags
     maker <- listToMaybe $ dropWhile (~/= "<foaf:Person>") $ dropWhile (~/= "<foaf:maker>") tags
     return $ Post { pId = sanitizeId $ fromAttrib "rdf:about" post
                   , pUser = sanitizeId $ fromAttrib "rdf:about" maker
                   }

putPost :: FilePath -> MaybeT Redis ()
putPost f = do a <- tryReadFile f
               p <- MaybeT $ return $ parsePost a
               lift $ hset (E.encodeUtf8 $ pId p) "user" (E.encodeUtf8 $ pUser p)
               lift $ sadd (E.encodeUtf8 $ pUser p `T.append` "%posts") [E.encodeUtf8 $ pId p]
               lift $ sadd "%posts" [E.encodeUtf8 $ pId p]
               return ()

main = extract $ \f->do runMaybeT $ putPost f
                        return ()
