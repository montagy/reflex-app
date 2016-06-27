{-# LANGUAGE OverloadedStrings #-}
module Api (
  fakeGetData,
  getTopicList,
  postComment
) where

import Common.Types
import Reflex
import Reflex.Dom
import Data.Bson (genObjectId)
import Control.Monad.IO.Class
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

topics :: [Topic]
topics = [Topic Nothing "t1" "content1" Nothing, Topic Nothing "t2" "content2" Nothing]

fakeGetData :: MonadWidget t m => String -> m (Event t Topic)
fakeGetData s = do
  let
    comment = Just [Comment Nothing Agree "打一架", Comment Nothing Agree "说的好", Comment Nothing Against "放狗屁"]
    art0 = Topic Nothing "Fake Title" "Fake content" comment
    art1 = topics !! 0
    art2 = topics !! 1
    art  = case s of
            "1" -> art1
            "2" -> art2
            _ -> art0

  e <- getPostBuild
  delay 1 (art <$ e)

getTopicList :: MonadWidget t m => m (Event t [Topic])
getTopicList = do
  event <- delay 1 =<< getPostBuild
  pure $ topics <$ event

postComment :: MonadWidget t m  => Comment -> m (Event t Comment)
postComment c = do
  oid <- liftIO genObjectId
  e <- delay 1 =<< getPostBuild
  pure $ c{commentId = Just oid} <$ e

