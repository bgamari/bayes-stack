{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Data.List.Split
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

import Control.Concurrent.ParallelIO
import Control.Monad (guard, liftM, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Concurrent.MVar

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
       res <- lift $ hget postId "thread"
       case res of
         Left _ -> error "Fail"
         Right Nothing -> liftIO $ putStrLn $ "No thread: "++show postId
         Right (Just threadId) -> do
           title <- lift $ hget threadId "title"
           case title of
                Left _ -> error "WTF"
                Right Nothing -> liftIO $ putStrLn $ "No title: "++show threadId
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
     
     count <- newMVar 0 :: IO (MVar Int)
     let handlePerson (i,p) = do
           kwds <- getUserKeywords stopWords p
           case kwds of
                  []  -> return ()
                  kws -> do forM_ kws $ zincrby (p `BS.append` "%keywords") 1 . E.encodeUtf8
                            forM_ kws $ zincrby "%keywords" 1 . E.encodeUtf8
                            sadd "%peopleWithKeywords" [p]
                            liftIO $ when (i `mod` 100 == 0) $ modifyMVar_ count $ return . (100+)
   
     Right people <- runRedis conn $ smembers "%peopleWithPosts"
     let chunks = splitEvery 100 people
     parallel_ $ map (\chunk->runRedis conn $ do c <- liftIO $ readMVar count 
                                                 liftIO $ print c
                                                 mapM_ handlePerson $ zip [1..] chunk
                     ) chunks
     stopGlobalPool

