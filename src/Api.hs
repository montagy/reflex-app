{-# LANGUAGE OverloadedStrings #-}
module Api (
  getBlogs,
  fakeGetData
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

fakeGetData :: MonadWidget t m => m (Event t Topic)
fakeGetData = do
  let
    comment = Just [Comment Agree "打一架", Comment Agree "说的好", Comment Against "放狗屁"]
    art = Topic "Fake Title" "Fake content" comment

  event <- delay 1 =<< getPostBuild
  return $ art <$ event
