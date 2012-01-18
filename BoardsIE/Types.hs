{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveGeneric  #-}
module Types where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Set (Set)

import GHC.Generics


type UserId = Text
type ForumId = Text
type PostId = Text
type ThreadId = Text

data User = User { uId :: UserId
                 , uNick :: Text
                 , uKnows :: Set UserId }
            deriving (Show, Eq, Generic)

data Forum = Forum { fId :: ThreadId
                   , fTitle :: Text
                   , fParent :: Maybe ForumId
                   , fChildren :: Set ForumId
                   , fThreads :: Set ThreadId
                   }
             deriving (Show, Eq, Generic)

data Post = Post { pId :: PostId
                 , pUser :: UserId
                 }
            deriving (Show, Eq, Generic)

data Thread = Thread { tId :: ThreadId
                     , tTitle :: Text
                     , tForum :: ForumId
                     , tPosts :: Set PostId
                     }
            deriving (Show, Eq, Generic)

sanitizeId :: Text -> Text
sanitizeId = T.replace "http://www." "http://"
