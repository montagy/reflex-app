{-# LANGUAGE OverloadedStrings #-}
module Api (
  getBlogs,
  fakeGetData,
  getTopicList
) where

import Common.Types
import Reflex
import Reflex.Dom
{-import Data.Time-}
{-import Control.Monad.IO.Class (liftIO)-}

getBlogs:: MonadWidget t m => m (Event t [Article])
getBlogs = do
  let
    req = xhrRequest "GET" "http://localhost:3030/blogs" def
  event <- getPostBuild
  response <- performRequestAsync (req <$ event)
  return $ fmapMaybe decodeXhrResponse response

{-getUsers :: XhrRequest-}
{-getUsers = xhrRequest "GET" "http://localhost:3030/users" def-}

topics :: [Topic]
topics = [Topic (Just "1") "t1" "content1" Nothing, Topic (Just "2") "t2" "content2" Nothing]

fakeGetData :: MonadWidget t m => String -> m (Event t Topic)
fakeGetData s = do
  let
    comment = Just [Comment Agree "打一架", Comment Agree "说的好", Comment Against "放狗屁"]
    art0 = Topic (Just "Fake id") "Fake Title" "Fake content" comment
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
  return $ topics <$ event
