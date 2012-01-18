{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
 
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO

import Text.HTML.TagSoup
import Database.Redis
import Types
import Extract

parseThread :: Text -> Maybe Thread
parseThread thread = 
  do let tags = parseTags thread
     thread <- listToMaybe $ dropWhile (~/= "<sioc:Thread>") tags
     title <- listToMaybe $ dropWhile (not . isTagText) 
              $ dropWhile (~/= "<dc:title>")
              $ dropWhile (~/= "<sioc:Thread>") tags
     forum <- listToMaybe $ dropWhile (~/= "<sioc:Forum>") tags
     let posts = map (sanitizeId . fromAttrib "rdf:about")
                 $ filter (isTagOpenName "sioc:Post") tags
     return $ Thread { tId = sanitizeId $ fromAttrib "rdf:about" thread
                     , tTitle = fromTagText title
                     , tForum = sanitizeId $ fromAttrib "rdf:about" forum
                     , tPosts = S.fromList posts
                     }

putThread :: FilePath -> MaybeT Redis ()
putThread f =
  do a <- tryReadFile f
     t <- MaybeT $ return $ parseThread a
     lift $ hmset (E.encodeUtf8 $ tId t) [ (E.encodeUtf8 "title", E.encodeUtf8 $ tTitle t)
                                         , (E.encodeUtf8 "forum", E.encodeUtf8 $ tForum t)
                                         ]
     lift $ sadd (E.encodeUtf8 $ tId t `T.append` "%posts") $ map E.encodeUtf8 $ S.toList $ tPosts t
     lift $ sadd "%threads" [E.encodeUtf8 $ tId t]
     return ()

main = extract $ \f->do runMaybeT $ putThread f
                        return ()
