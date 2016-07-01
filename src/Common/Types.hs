{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Types where

import Data.Aeson
import qualified Data.Aeson as A
import GHC.Generics
import Data.Text (unpack, pack)
import Data.Time
import Data.Bson
import Control.Monad

instance ToJSON ObjectId where
  toJSON oid@(Oid x y) = A.String . pack $ show oid
instance FromJSON ObjectId where
  parseJSON oid@(A.String v) = return . read . unpack $ v
  parseJSON _ = mzero

data Topic = Topic
  { topicId :: Maybe ObjectId
  , topicTitle :: String
  , topicContent :: String
  , topicComments :: Maybe [Comment]
  } deriving (Eq, Show, Generic)

data Comment = Comment
  { commentId :: Maybe ObjectId
  , commentTopic :: String
  , commentSide :: CommentSide
  , commentContent :: String
  } deriving (Eq, Show, Generic)

data CommentSide = Agree | Against deriving (Enum, Ord, Eq, Show, Generic, Read)

instance ToJSON CommentSide
instance FromJSON CommentSide
instance ToJSON Comment
instance FromJSON Comment
instance ToJSON Topic
instance FromJSON Topic
