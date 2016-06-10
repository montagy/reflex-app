{-# LANGUAGE OverloadedStrings #-}
module Lib (
  app
) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T
import Control.Monad ((<=<))
import Css (bs)

app :: IO ()
app = mainWidgetWithCss bs $
  elClass "div" "main" $ do
    elClass "div" "header" navWidget
    elClass "div" "content"  $ getXhrContent getUsers

getXhrContent :: MonadWidget t m => XhrRequest -> m ()
getXhrContent req = do
  let
      eitherToText eRes = case eRes of
                            Left _ -> Just "The server cant connect"
                            Right res -> _xhrResponse_responseText res
  event <- getPostBuild
  response <- fmap eitherToText <$> performRequestAsyncWithError (req <$ event)
  dynText <=< holdDyn "Loading.." $ fmapMaybe (T.unpack <$>) response

getUsers :: XhrRequest
getUsers = xhrRequest "GET" "http://localhost:3030/users" def

navWidget :: MonadWidget t m => m ()
navWidget =
  elClass "nav" "navbar" $
    elClass "ul" "list" $ do
      el "li" $ elAttr "a" ("href" =: "#") $ text "Home"
      el "li" $ elAttr "a" ("href" =: "#") $ text "About"
      el "li" $ elAttr "a" ("href" =: "#") $ text "Concat"
