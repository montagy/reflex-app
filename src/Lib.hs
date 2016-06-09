{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Lib (
  app
) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T
import Control.Monad ((<=<))

app :: IO ()
app = mainWidget $
  elClass "div" "main" $ do
    elClass "div" "header" $ text "This is Header"
    elClass "div" "content"  getXhrContent

getXhrContent :: forall t m. MonadWidget t m => m ()
getXhrContent = do
  let req = xhrRequest "GET" "http://localhost:3030/users" def

  event <- getPostBuild
  response <- fmap (fmap _xhrResponse_responseText) $ performRequestAsync $ req <$ event
  dynText <=< holdDyn "Loading.." $ fmapMaybe (T.unpack <$>) response
