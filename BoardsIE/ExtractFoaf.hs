{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveGeneric  #-}

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

-- * FOAF
parseFoaf :: Text -> User
parseFoaf foaf =
  let tags = parseTags foaf
  in User { uId = sanitizeId $ fromAttrib "rdf:about" $ head
                  $ dropWhile (~/= "<foaf:Person>") tags
          , uNick = fromTagText $ head
                    $ tail $ dropWhile (~/= "<foaf:nick>") tags
          , uKnows = S.fromList
                     $ map (sanitizeId . fromAttrib "rdf:about" . head . dropWhile (~/= "<foaf:Person>"))
                     $ sections (~== "<foaf:knows>") tags
          }

putUser :: FilePath -> MaybeT Redis ()
putUser f = do a <- tryReadFile f
               u <- return $ parseFoaf a
               lift $ hset (E.encodeUtf8 $ uId u) "nick" (E.encodeUtf8 $ uNick u)
               lift $ sadd (E.encodeUtf8 $ uId u `T.append` "%knows") $ map E.encodeUtf8 $ S.toList $ uKnows u
               lift $ sadd "%people" [E.encodeUtf8 $ uId u]
               lift $ when (not $ S.null $ uKnows u) $
                 sadd "%peopleWithFriends" [E.encodeUtf8 $ uId u] >> return ()

main = extract $ \f->do runMaybeT $ putUser f
                        return ()
