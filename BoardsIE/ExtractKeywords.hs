{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Data.Hashable
import qualified Data.Text as T
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Char

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES

import Control.Monad (guard, liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

import Control.Concurrent.ParallelIO.Global
import Database.MongoDB
import BayesStack.Models.Topic.Types
import BayesStack.UniqueKey

type Keyword = String
type UserId = String
type PostId = String
type ThreadId = String

getStopWords :: IO (Set T.Text)
getStopWords = liftM (S.fromList . map T.pack . filter (\w->length w >= 2) . words) $
                readFile "BoardsIE/stopwords.txt"

getUserKeywords :: Set T.Text -> UserId -> Action IO [Keyword]
getUserKeywords stopWords user =
  do cursor <- find (select ["user" =: user] "posts")
     execWriterT $ fetch cursor
  where fetch :: Cursor -> WriterT [Keyword] (Action IO) ()
        fetch c = do d <- lift $ next c
                     case d of
                       Just doc -> do
                         runMaybeT $ do let id = "_id" `at` doc :: PostId
                                        thread <- MaybeT $ lift $ findOne (select ["posts" =: id] "threads")
                                        let title = "title" `at` thread :: String
                                            titleWords = map T.unpack
                                                         $ filter (`S.notMember` stopWords)
                                                         $ filter (\w->T.compareLength w 2 == GT)
                                                         $ T.words
                                                         $ T.toLower
                                                         $ T.filter (\c->isAlpha c || isSpace c)
                                                         $ T.pack title :: [Keyword]
                                        lift $ tell titleWords
                         fetch c
                       Nothing  -> return ()

main =
  do pipe <- runIOE $ connect (Host "avon-2" (PortNumber 27025))
     stopWords <- liftIO getStopWords

     Right users <- access pipe master "boardsie" $ do
                          cursor <- find (select [] "users")
                          liftM (map ("_id" `at`)) $ rest cursor
     let hello u = do liftIO $ print u
                      access pipe master "boardsie" $ do
                        kws <- getUserKeywords stopWords u
                        repsert (select ["_id" =: u] "userKeywords")
                                [ "$pushAll" =: ["keywords" =: kws]]
     parallel_ $ map hello users
     --mapM_ hello users
     close pipe
     stopGlobalPool

