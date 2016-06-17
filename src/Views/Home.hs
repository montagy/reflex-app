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
    topic eTopic

    el "hr" (return ())
    el "p" $ text "This is a new text line"

initialTopic :: Topic
initialTopic = Topic "fake" "fake" Nothing

topicInput :: MonadWidget t m => m (Event t Topic)
topicInput = do
  rec
      titleInput  <- textInput $ def & setValue .~ ("" <$ result)
      contentInput <- textInput $ def & setValue .~ ("" <$ result)
      submit <- button "Submit"
      dTopic <- Topic `mapDyn` value titleInput `apDyn` value contentInput `apDyn` constDyn Nothing
      let result = ffilter (\t -> (not . null . topicTitle) t && (not . null . topicContent) t ) $ attachWith const (current dTopic) submit
  return result

topic :: MonadWidget t m => Event t Topic -> m ()
topic submitTopic =
  divClass "topic" $ do
    divClass "topic__main" $ display =<< holdDyn initialTopic submitTopic
    dToggle <- toggle False =<< button "Add New"
    attrib <- mapDyn (\t -> if t then "style" =: "display:none" else "style" =: "display:block") dToggle
    _ <- elDynAttr "div" attrib commentInput
    pure ()

commentInput :: MonadWidget t m => m ()
commentInput = do
  _ <- textInput def
  _ <- textInput def
  _ <- button "Submit"
  pure ()
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

