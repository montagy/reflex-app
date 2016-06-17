{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Views.Home (
  page
) where


import Reflex
import Reflex.Dom
import Control.Monad
{-import Data.Time-}
{-import Control.Monad.IO.Class-}

import qualified Data.Text as T
import Common.Types
import Api (fakeGetBlog)

page :: MonadWidget t m => m ()
page =
  elClass "div" "main" $ do
    el  "header" navWidget
    eArts <- fakeGetBlog
    dView <- holdDyn loading $ article <$> eArts
    void $ dyn dView

    dToggle <- toggle False =<< button "toggle"
    attrib <- mapDyn (\t -> if t then "style" =: "display:none" else "style" =: "display:block") dToggle
    eTopic <- elDynAttr "div" attrib topicInput
    divClass "topic" $
      display =<< holdDyn (Topic "fake" "fake" Nothing) eTopic

    el "hr" (return ())
    el "p" $ text "This is a new text line"

topicInput :: MonadWidget t m => m (Event t Topic)
topicInput = do
  rec
      titleInput  <- textInput $ def & setValue .~ ("" <$ result)
      contentInput <- textInput $ def & setValue .~ ("" <$ result)
      submit <- button "Submit"
      dTopic <- Topic `mapDyn` value titleInput `apDyn` value contentInput `apDyn` constDyn Nothing
      let result = ffilter (\topic -> (not . null . topicTitle) topic && (not . null . topicContent) topic ) $ attachWith const (current dTopic) submit
  return result

navWidget :: MonadWidget t m => m ()
navWidget =
  el "nav"  $
    elClass "ul" "list" $ do
      el "li" $ elAttr "a" ("href" =: "/") $ text "Home"
      el "li" $ elAttr "a" ("href" =: "/post") $ text "Post"

loading :: MonadWidget t m => m ()
loading = divClass "loading" $ text "loading..."

article :: MonadWidget t m => Article -> m ()
article art =
  divClass "article" $ do
    text $ T.unpack . title $ art
    text $ show . createTime $ art
    pure ()
{-
 -articleInfoV :: MonadWidget t m => Dynamic t Article-> m ()
 -articleInfoV dArt =
 -  elClass "li" "blog_info" $ do
 -    dynTitle <- mapDyn (T.unpack . title) dArt
 -    dynTime <- mapDyn (show . createTime) dArt
 -    el "div" $ dynText dynTitle
 -    el "div" $ dynText dynTime
 -    pure ()
 -
 -articleInfoListV :: MonadWidget t m => Event t [Article] -> m ()
 -articleInfoListV eArts = do
 -  dynArts :: Dynamic t [Article] <- holdDyn [] eArts
 -  _ <- simpleList dynArts articleInfoV
 -  pure ()
 -}

