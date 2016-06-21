{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Views.Home (
  page
) where


import Reflex
import Reflex.Dom
import Control.Monad
import Data.Monoid
import Data.Map (Map)

import qualified Data.Text as T
import Common.Types
import Api (fakeGetData)

        {-dToggle <- toggle False =<< button "toggle"-}
        {-attrib <- mapDyn (\t -> if t then "style" =: "display:none" else "style" =: "display:block") dToggle-}
        {-eTopic <- elDynAttr "div" attrib topicInput-}
        {-topic eTopic-}
page :: MonadWidget t m => m ()
page = do
  header
  divClass "container" $ do
    eData <- fakeGetData
    dView <- holdDyn loading $ topicView <$> eData
    divClass "topic_wrapper" $
      void $ dyn dView
    divClass "xiaohua_wrapper radius" $
      divClass "xiaohua" $ text "滚动的笑话"
  footer

footer :: MonadWidget t m => m ()
footer = do
  elClass "footer" "clear" $ el "div" $ text "This is a new text line"
  pure ()

header :: MonadWidget t m => m ()
header = do
  el "header" navWidget
  pure ()

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

{-topic :: MonadWidget t m => Event t Topic -> m ()-}
{-topic submitTopic =-}
  {-divClass "topic" $ do-}
    {-divClass "topic__main" $ display =<< holdDyn initialTopic submitTopic-}
    {-dToggle <- toggle False =<< button "Add New"-}
    {-attrib <- mapDyn (\t -> if t then "style" =: "display:none" else mempty) dToggle-}
    {-_ <- elDynAttr "div" attrib commentInput-}
    {-pure ()-}

navWidget :: MonadWidget t m => m ()
navWidget =
  elClass "nav" "container" $
    divClass "nav__content" $ text "吵架与看笑话"

loading :: MonadWidget t m => m ()
loading = divClass "loading" $ text "loading..."

topicView :: MonadWidget t m => Topic -> m ()
topicView Topic{..}= do
  divClass "topic radius" $ do
    divClass "topic__title" $ text topicTitle
    divClass "topic__content" $ text topicContent
  divClass "comment radius" $
    case topicComments of
      Nothing -> pure ()
      Just comments -> commentsView comments

  pure ()

comment :: MonadWidget t m => Dynamic t Comment -> m ()
comment c = do
  content <- mapDyn commentContent c
  divClass "comment__item" $ dynText content
  pure ()
commentsView :: MonadWidget t m => [Comment] -> m ()
commentsView comments = do
  rec
    let eSubmit = ffilter (== keycodeEnter) $ _textArea_keypress area
        dContent = _textArea_value area
        dSide = _dropdown_value drop
        selectList = Agree =: "agree" <> Against =: "against"

    dNewComment <- combineDyn Comment dSide dContent
    let eRealSubmit = tag (current dNewComment ) eSubmit

    dComments <- foldDyn (:) comments eRealSubmit
    dAgreeComments <- mapDyn (filter (\x -> commentSide x == Agree)) dComments
    dAgainstComments <- mapDyn (filter (\x -> commentSide x == Against)) dComments
    divClass "comment__left" $
      simpleList dAgreeComments comment
    divClass "comment__right" $
      simpleList dAgainstComments comment

    (drop, area) <- divClass "comment__input clear" $ do
      drop' <- dropdown Agree (constDyn selectList) def
      area' <- textArea $ def & setValue .~ ("" <$ eSubmit)
      pure (drop', area')
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

