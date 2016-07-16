{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Types where

import Data.Aeson
import qualified Data.Aeson as A
import GHC.Generics
import Data.Text (Text, unpack, pack)
import Data.Bson
import Control.Monad

instance ToJSON ObjectId where
  toJSON oid = A.String . pack $ show oid
instance FromJSON ObjectId where
  parseJSON (A.String v) = return . read . unpack $ v
  parseJSON _ = mzero

data Topic = Topic
  { topicId :: Maybe ObjectId
  , topicTitle :: Text
  , topicContent :: Text
  , topicComments :: [Comment]
  } deriving (Eq, Show, Generic)

data Comment = Comment
  { commentId :: Maybe ObjectId
  , commentTopicId :: ObjectId
  , commentSide :: CommentSide
  , commentContent :: Text
  } deriving (Eq, Show, Generic)

data CommentSide = Agree | Against deriving (Enum, Ord, Eq, Show, Generic, Read)

instance ToJSON CommentSide
instance FromJSON CommentSide
instance ToJSON Comment
instance FromJSON Comment
instance ToJSON Topic
instance FromJSON Topic

type UserName = Text
type Password = Text
data UserInfo = UserInfo
  { infoName :: UserName
  } deriving (Eq, Show, Generic)
data User = User
  { username :: UserName
  , password :: Password
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
instance ToJSON UserInfo
instance FromJSON UserInfo

