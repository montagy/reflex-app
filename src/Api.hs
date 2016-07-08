{-# LANGUAGE OverloadedStrings #-}
module Api (
  fakeGetData,
  getTopicList,
  postComment
) where

import Common.Types
import Reflex
import Reflex.Dom
import Data.Bson
import Control.Monad.IO.Class
import Data.Monoid
import Data.String
{-import Data.Time-}
{-import Control.Monad.IO.Class (liftIO)-}

{-getBlogs:: MonadWidget t m => m (Event t [Article])-}
{-getBlogs = do-}
  {-let-}
    {-req = xhrRequest "GET" "http://localhost:3030/blogs" def-}
  {-event <- getPostBuild-}
  {-response <- performRequestAsync (req <$ event)-}
  {-return $ fmapMaybe decodeXhrResponse response-}

{-getUsers :: XhrRequest-}
{-getUsers = xhrRequest "GET" "http://localhost:3030/users" def-}
host :: IsString a => a
host = "http://localhost:3030/"

fakeGetData :: MonadWidget t m => Maybe ObjectId -> m (Event t Topic)
fakeGetData s = do
  let req =
        case s of
            Nothing -> xhrRequest "GET" (host <> "topic/newest") def
            Just id' -> xhrRequest "GET" (host <> "topic/" <> show id') def
  e <- getPostBuild
  fmapMaybe decodeXhrResponse <$> performRequestAsync (req <$ e)

getTopicList :: MonadWidget t m => m (Event t [Topic])
getTopicList = do
  let req = xhrRequest "GET" (host <> "topics") def
  event <- getPostBuild
  fmapMaybe decodeXhrResponse <$> performRequestAsync (req <$ event)


postComment :: MonadWidget t m  => Comment -> m (Event t Comment)
postComment c = do
  oid <- liftIO genObjectId
  e <- delay 1 =<< getPostBuild
  pure $ c{commentId = Just oid} <$ e

