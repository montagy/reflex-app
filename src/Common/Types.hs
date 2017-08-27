{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import Data.Aeson
import qualified Data.Aeson as A
import GHC.Generics
import Data.Text (Text, pack, unpack)
import Control.Monad (mzero)
import Data.Bson
import Data.Default
import Common.ObjectId

type UserName = Text
data Topic = Topic
  { topicId :: Maybe ObjectId
  , topicUser :: Maybe UserName
  , topicTitle :: Text
  , topicContent :: Text
  , topicComments :: [Comment]
  } deriving (Eq, Show, Generic)

instance Default Topic where
  def = Topic
    { topicId = Nothing
    , topicUser = Nothing
    , topicTitle = ""
    , topicContent = ""
    , topicComments = []
    }
data Comment = Comment
  { commentId :: Maybe ObjectId
  , commentTopicId :: Maybe ObjectId
  , commentUser :: Maybe UserName
  , commentSide :: CommentSide
  , commentContent :: Text
  } deriving (Eq, Show, Generic)

instance Default Comment where
  def = Comment
    { commentId = Nothing
    , commentTopicId = Nothing
    , commentUser = Nothing
    , commentSide = Agree
    , commentContent = ""
    }
data CommentSide = Agree | Against deriving (Enum, Ord, Eq, Show, Generic, Read)

instance ToJSON CommentSide
instance FromJSON CommentSide
instance ToJSON Comment
instance FromJSON Comment
instance ToJSON Topic
instance FromJSON Topic

data UserInfo = UserInfo
  { infoName :: UserName
  } deriving (Eq, Show, Generic)
data User = User
  { username :: UserName
  , password :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
instance ToJSON UserInfo
instance FromJSON UserInfo

data Token = Token
  { token :: Text
  } deriving (Generic)

instance ToJSON Token
instance FromJSON Token
