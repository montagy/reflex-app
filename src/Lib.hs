{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Lib (
  app
) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T
{-import qualified Data.Map as M-}
import Reflex.Dom.Contrib.Router

import Css (bs)
import Api
import Common.Types

router :: MonadWidget t m => String -> m (Event t String)
router "/" = home
router "/post" = post
router _ = home

app :: IO ()
app = routeSite router

home :: MonadWidget t m => m (Event t String)
home =
  elClass "div" "main" $ do
    el  "header" navWidget
    eArts <- getBlogs
    elClass "div" "content"  $ elClass "ul" "blogs" $ articleInfoListV eArts
    pure never

post :: MonadWidget t m => m (Event t String)
post =
  elClass "div" "main" $ do
    el "header" navWidget
    postBlogV
    return never


navWidget :: MonadWidget t m => m ()
navWidget =
  el "nav"  $
    elClass "ul" "list" $ do
      el "li" $ elAttr "a" ("href" =: "/") $ text "Home"
      el "li" $ elAttr "a" ("href" =: "/post") $ text "Post"

articleInfoV :: MonadWidget t m => Dynamic t Article-> m ()
articleInfoV dArt =
  elClass "li" "blog_info" $ do
    dynTitle <- mapDyn (T.unpack . title) dArt
    dynTime <- mapDyn (show . createTime) dArt
    el "div" $ dynText dynTitle
    el "div" $ dynText dynTime
    pure ()

articleInfoListV :: MonadWidget t m => Event t [Article] -> m ()
articleInfoListV eArts = do
  dynArts :: Dynamic t [Article] <- holdDyn [] eArts
  _ <- simpleList dynArts articleInfoV
  pure ()

postBlogV :: MonadWidget t m => m ()
postBlogV =
  el "form" $ do
    eTitle <- textInput def
    eContent <- textArea def
    _ <- button "Submit"
    pure ()
