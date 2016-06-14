module Api (
  getBlogs
) where

import Common.Types
import Reflex
import Reflex.Dom

getBlogs:: MonadWidget t m => m (Event t [Article])
getBlogs = do
  let
    req = xhrRequest "GET" "http://localhost:3030/blogs" def
  event <- getPostBuild
  response <- performRequestAsync (req <$ event)
  return $ fmapMaybe decodeXhrResponse response

{-getUsers :: XhrRequest-}
{-getUsers = xhrRequest "GET" "http://localhost:3030/users" def-}
