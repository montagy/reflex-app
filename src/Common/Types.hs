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
