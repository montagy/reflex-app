{-# LANGUAGE OverloadedStrings #-}
module Api where

import Common.Types
import Reflex
import Reflex.Dom
import Data.Bson
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
getNewestTopic :: MonadWidget t m => m (Event t Topic)
getNewestTopic = do
  let req = xhrRequest "GET" (host <> "topic/newest") def
  e <- getPostBuild
  fetchByEvent (req <$ e)

getTopicList :: MonadWidget t m => m (Event t [Topic])
getTopicList = do
  let req = xhrRequest "GET" (host <> "topics") def
  event <- getPostBuild
  fetchByEvent (req <$ event)

fetchTopicList :: MonadWidget t m => Event t a -> m (Event t [Topic])
fetchTopicList e = do
  let req = xhrRequest "GET" (host <> "topics") def
  fetchByEvent (req <$ e)

postComment :: MonadWidget t m  => Comment -> m (Event t [Comment])
postComment c = do
  let req = postJson (host <> "comment") c

  e <- getPostBuild
  fetchByEvent (req <$ e)

postComment'  :: MonadWidget t m => Event t Comment -> m (Event t [Comment])
postComment'  e = do
  let req  = postJson (host <> "comment")

  fetchByEvent (req <$> e)

postTopic :: MonadWidget t m => Event t Topic -> m (Event t Topic)
postTopic e = do
  let req = postJson (host <> "topic")

  fetchByEvent (req <$> e)
