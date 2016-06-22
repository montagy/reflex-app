{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Views.Home (
  page
) where


import Reflex
import Reflex.Dom
import Control.Monad
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map

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

comment :: MonadWidget t m => Int -> Dynamic t Comment -> m ()
comment _ c = do
  content <- mapDyn commentContent c
  divClass "comment__item" $ dynText content
  pure ()

commentEntry :: MonadWidget t m => m (Event t Comment)
commentEntry = divClass "comment__input clear" $ do
  rec
    let
        eContent = fmapMaybe maybeStrip $ tag (current $ _textArea_value area) eSubmit
        dSide = _dropdown_value drop
        eNewComment = attachDynWith Comment dSide eContent
        selectList = Agree =: "agree" <> Against =: "against"

    drop <- dropdown Agree (constDyn selectList) def
    area <- textArea $ def & setValue .~ ("" <$ eSubmit)
    eSubmit <- button "submit"
  return eNewComment

commentsView :: MonadWidget t m => [Comment] -> m ()
commentsView comments = do
  rec
    dComments <- foldDyn insertNew_ (Map.fromList $ zip [1..] comments) eNewComment
    dAgreeComments <- mapDyn (Map.filter (\x -> commentSide x == Agree)) dComments
    dAgainstComments <- mapDyn (Map.filter (\x -> commentSide x == Against)) dComments
    divClass "comment__left" $
      listWithKey dAgreeComments comment
    divClass "comment__right" $
      listWithKey dAgainstComments comment

    eNewComment <- commentEntry
    {-divClass "message" $ dynText dMsg-}
  pure ()

stripString :: String -> String
stripString  = T.unpack . T.strip . T.pack

maybeStrip :: String -> Maybe String
maybeStrip (stripString -> "") = Nothing
maybeStrip (stripString -> trimmed) = Just trimmed
-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m
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

