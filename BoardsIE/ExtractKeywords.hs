{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Data.Hashable
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Char

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as E

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.ByteString as BS
import Data.Maybe

import Control.Monad (guard, liftM, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

import Types
import Database.Redis

type Keyword = Text

getStopWords :: IO (Set Text)
getStopWords = liftM (S.fromList . map T.pack . filter (\w->length w >= 2) . words)
               $ readFile "stopwords.txt"

getUserKeywords :: Set Text -> BS.ByteString -> Redis [Keyword]
getUserKeywords stopWords user =
  do Right posts <- smembers $ user `BS.append` "%posts"
     execWriterT $ forM_ posts $ \postId->do
       Right (Just threadId) <- lift $ hget "thread" postId
       title <- lift $ hget "title" threadId
       case title of
            Left _ -> return ()
            Right Nothing -> return ()
            Right (Just t) ->
              let titleWords = filter (`S.notMember` stopWords)
                               $ filter (\w->T.compareLength w 2 == GT)
                               $ T.words
                               $ T.toLower
                               $ T.filter (\c->isAlpha c || isSpace c)
                               $ E.decodeUtf8 t :: [Keyword]
              in tell titleWords

connectInfo = defaultConnectInfo {connectHost="blake"}

main =
  do conn <- connect $ connectInfo
     stopWords <- liftIO getStopWords
     runRedis conn $ do
       Right hi <- keys "*%keywords"
       del hi
       del ["%peopleWithKeywords"]

       Right people <- smembers "%peopleWithPosts"
       forM_ (zip [1..] people) $ \(i,p)->do
         keywords <- getUserKeywords stopWords p
         guard (not $ empty keywords)
         forM_ keywords $ zincrby (p `BS.append` "%keywords") 1 . E.encodeUtf8
         sadd "%peopleWithKeywords" [p]
         liftIO $ when (i `mod` 1000 == 0) $ print i

