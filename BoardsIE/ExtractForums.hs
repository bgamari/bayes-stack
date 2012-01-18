{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
 
import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import Data.Text (Text)
import qualified Data.Text as T

import Text.HTML.TagSoup
import Database.Redis
import Types
import Extract

parseForum :: Text -> Forum
parseForum forum = 
  let tags = dropWhile (~/= "<sioct:MessageBoard>") $ parseTags forum
      children = map (takeWhile (~/= "</sioc:parent_of>")) $ sections (~== "<sioc:parent_of") tags
  in Forum { fId = sanitizeId . fromAttrib "rdf:about" $ head
                   $ dropWhile (~/= "<sioct:MessageBoard>") tags
           , fTitle = fromTagText $ head $ dropWhile (not . isTagText) 
                      $ dropWhile (~/= "<dc:title>") tags
           , fParent = fmap (sanitizeId . fromAttrib "rdf:about") $ listToMaybe
                       $ dropWhile (~/= "<sioc:Forum>")
                       $ dropWhile (~/= "<sioc:has_parent>") tags
           , fChildren = S.fromList 
                         $ mapMaybe (fmap (sanitizeId . fromAttrib "rdf:about") . listToMaybe
                                     . dropWhile (~/= "<sioc:Forum>"))
                         $ children
           , fThreads = S.fromList
                        $ mapMaybe (fmap (sanitizeId . fromAttrib "rdf:about") . listToMaybe
                                    . dropWhile (~/= "<sioc:Thread>"))
                        $ children
           }

putForum :: FilePath -> MaybeT Redis ()
putForum f =
  do a <- tryReadFile f
     t <- return $ parseForum a
     lift $ hmset (E.encodeUtf8 $ fId t) [ (E.encodeUtf8 "title", E.encodeUtf8 $ fTitle t) ]
     --lift $ when (isJust $ fParent t) $ hset (E.encodeUtf8 $ fId t) (E.encodeUtf8 "parent") (E.encodeUtf8 $ fromJust $ fParent t)
     lift $ sadd (E.encodeUtf8 $ fId t `T.append` "%threads") $ map E.encodeUtf8 $ S.toList $ fThreads t
     lift $ sadd "%forums" $ [E.encodeUtf8 $ fId t]
     return ()

main = extract $ \f->do runMaybeT $ putForum f
                        return ()
