{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib (
  app
) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T
import Control.Monad ((<=<))
import qualified Data.Map as M

import Css (bs)
import Common.Types

app :: IO ()
app = mainWidgetWithCss bs $
  elClass "div" "main" $ do
    el  "header" navWidget
    elClass "div" "content"  $ getXhrContent getBlogs


getXhrContent :: MonadWidget t m => XhrRequest -> m ()
getXhrContent req = do
  let
      eitherToText eRes = case eRes of
                            Left _ -> Nothing
                            Right res -> _xhrResponse_responseText res
  event <- getPostBuild
  response <- performRequestAsync (req <$ event)
  {-dynText <=< holdDyn "Loading.." $ fmapMaybe (T.unpack <$>) response-}
  articleInfoListV $ fmapMaybe decodeXhrResponse response

getUsers :: XhrRequest
getUsers = xhrRequest "GET" "http://localhost:3030/users" def
getBlogs :: XhrRequest
getBlogs = xhrRequest "GET" "http://localhost:3030/blogs" def

navWidget :: MonadWidget t m => m ()
navWidget =
  el "nav"  $
    elClass "ul" "list" $ do
      el "li" $ elAttr "a" ("href" =: "#") $ text "Home"
      el "li" $ elAttr "a" ("href" =: "#") $ text "About"
      el "li" $ elAttr "a" ("href" =: "#") $ text "Concat"

articleInfoV :: MonadWidget t m => Dynamic t Article-> m ()
articleInfoV dArt =
  elClass "div" "blog_info" $ do
    dynTitle <- mapDyn (T.unpack . title) dArt
    el "div" $ dynText dynTitle
    el "div" $ text "time here"

articleInfoListV :: MonadWidget t m => Event t [Article] -> m ()
articleInfoListV eArts = do
  dynArts :: Dynamic t (M.Map Int Article)<- holdDyn M.empty $ M.fromList . zip [2..] <$> eArts
  _ <- listWithKey dynArts (\_ d -> articleInfoV d)
  pure ()
