{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Types where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Time

data Article = Article
  { _id :: Maybe Text
  , title :: Text
  , content :: Text
  , createTime :: Maybe UTCTime
  } deriving  (Eq, Show, Generic)

instance ToJSON Article
instance FromJSON Article

data Topic = Topic
  { topicTitle :: String
  , topicContent :: String
  , topicComment :: Maybe [Comment]
  } deriving (Eq, Show, Generic)

data Comment = Comment
  { commentSide :: CommentSide
  , commentContent :: String
  } deriving (Eq, Show, Generic)

data CommentSide = Agree | Against deriving (Eq, Show, Generic)

instance ToJSON CommentSide
instance FromJSON CommentSide
instance ToJSON Comment
instance FromJSON Comment
instance ToJSON Topic
instance FromJSON Topic
