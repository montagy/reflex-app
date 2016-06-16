{-# LANGUAGE OverloadedStrings #-}
module Api (
  getBlogs,
  fakeGetBlog
) where

import Common.Types
import Reflex
import Reflex.Dom
import Data.Time
import Control.Monad.IO.Class (liftIO)

getBlogs:: MonadWidget t m => m (Event t [Article])
getBlogs = do
  let
    req = xhrRequest "GET" "http://localhost:3030/blogs" def
  event <- getPostBuild
  response <- performRequestAsync (req <$ event)
  return $ fmapMaybe decodeXhrResponse response

{-getUsers :: XhrRequest-}
{-getUsers = xhrRequest "GET" "http://localhost:3030/users" def-}

fakeGetBlog :: MonadWidget t m => m (Event t Article)
fakeGetBlog = do
  time <- liftIO getCurrentTime
  let art = Article (Just "ididi") "Fake Article" "Fake content" (Just time)
  event <- getPostBuild
  return $ art <$ event
