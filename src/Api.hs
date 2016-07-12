{-# LANGUAGE OverloadedStrings #-}
module Api where

import Common.Types
import Reflex
import Reflex.Dom
import Data.Bson
import Control.Monad.IO.Class
import Data.Monoid
import Data.String
import Data.Aeson (FromJSON)
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

fetchByEvent :: (MonadWidget t m, FromJSON b) => Event t XhrRequest -> m (Event t b)
fetchByEvent e =
  fmapMaybe decodeXhrResponse <$> performRequestAsync e

fetchTopic :: (MonadWidget t m) => Event t ObjectId -> m (Event t Topic)
fetchTopic e = do
  let req id' = xhrRequest "GET" (host <> "topic/" <> show id') def

  fetchByEvent $ req <$> e
fakeGetData :: MonadWidget t m => Maybe ObjectId -> m (Event t Topic)
fakeGetData s = do
  let req =
        case s of
            Nothing -> xhrRequest "GET" (host <> "topic/newest") def
            Just id' -> xhrRequest "GET" (host <> "topic/" <> show id') def
  e <- getPostBuild
  fetchByEvent (req <$ e)

getTopicList :: MonadWidget t m => m (Event t [Topic])
getTopicList = do
  let req = xhrRequest "GET" (host <> "topics") def
  event <- getPostBuild
  fetchByEvent (req <$ event)


postComment :: MonadWidget t m  => Comment -> m (Event t [Comment])
postComment c = do
  let req = postJson (host <> "comment") c

  e <- getPostBuild
  fetchByEvent (req <$ e)

