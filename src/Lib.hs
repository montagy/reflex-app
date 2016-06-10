{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
      eitherToText eRes = case eRes of
                            Left _ -> Just "The server cant connect"
                            Right res -> _xhrResponse_responseText res
  event <- getPostBuild
  afterDelay <- delay 5 $ req <$ event
  response <- fmap eitherToText <$> performRequestAsyncWithError afterDelay
  dynText <=< holdDyn "Loading.." $ fmapMaybe (T.unpack <$>) response
