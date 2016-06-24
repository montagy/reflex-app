{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
import Api

page :: MonadWidget t m => m ()
page = do
  header
  divClass "container" $ do
    rec
      {-dItem <- traceDyn "clicked" <$> holdDyn "" (show <$> eItemClick)-}
      eTopic' <- widgetHold (fakeGetData "inital") (fakeGetData . show <$> eItemClick)
      {-fake <- fakeGetData "inital"-}
      let eTopic = switchPromptlyDyn eTopic'
--begin
      {-eTopic <- fakeGetData "inital"-}
      eTopicList <- getTopicList
--data already
      divClass "topic_wrapper" $ topicView eTopic
      eItemClick <- divClass "xiaohua_wrapper raiuds" $ topicList eTopicList
    pure ()
  footer

topicList :: MonadWidget t m =>Event t [Topic] -> m (Event t Int)
topicList eTs = do
  let
      view :: (MonadWidget t m ) => Int -> Dynamic t Topic -> m (Event t Int)
      view k dT = do
        (dom, _) <- elAttr' "div" ("class" =: "xiaohua") $ dynText =<< mapDyn topicTitle dT
        pure $ k <$ domEvent Click dom

  dTs <- holdDyn Map.empty $  Map.fromList . zip [1..] <$> eTs
  selectEntry <- listWithKey dTs view
  switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry

initialTopic :: Topic
initialTopic = Topic Nothing "init title" "init content" Nothing

topicView :: MonadWidget t m => Event t Topic -> m ()
topicView eTopic = do
  dTopic <- holdDyn initialTopic eTopic
  divClass "topic radius" $ do
    divClass "topic__title" $ dynText =<< mapDyn topicTitle dTopic
    divClass "topic__content" $ dynText =<< mapDyn topicContent dTopic

  divClass "comment radius" (commentsView =<< mapDyn topicComments dTopic)
  pure ()

commentEntry :: MonadWidget t m => m (Event t Comment)
commentEntry = divClass "comment__input clear" $ do
  rec
    let
        eContent = fmapMaybe maybeStrip $ tag (current $ _textArea_value area) eSubmit
        dSide = _dropdown_value drop
        eNewComment = attachDynWith Comment dSide eContent
        selectList = Agree =: "agree" <> Against =: "against"

    drop <- dropdown Agree (constDyn selectList) $ def &
      attributes .~ constDyn ("class" =: "form-control")
    area <- textArea $ def & setValue .~ ("" <$ eSubmit) &
      attributes .~ constDyn (mconcat ["class" =: "form-control",
                                      "row" =: "3"])
    eSubmit <- do
      (e, _) <- elAttr' "button" (mconcat ["type" =: "button", "class" =: "form-control"]) $ text "Submit"
      pure $ domEvent Click e
  return eNewComment

commentsView :: MonadWidget t m =>Dynamic t (Maybe [Comment])-> m ()
commentsView dmComments = do
  let
    comment :: MonadWidget t m => Int -> Dynamic t Comment -> m ()
    comment _ c = do
      content <- mapDyn commentContent c
      divClass "comment__item" $ dynText content
      pure ()

  dComments' <- forDyn dmComments $
    \case
        Nothing -> Map.empty
        Just comments -> Map.fromList $ zip [1..] comments
  rec
    dComments <- combineDyn (foldr insertNew_)  dComments' =<< foldDyn (:) [] eNewComment
    dAgreeComments <- mapDyn (Map.filter (\x -> commentSide x == Agree)) dComments
    dAgainstComments <- mapDyn (Map.filter (\x -> commentSide x == Against)) dComments
    _ <- divClass "comment__left" $
      listWithKey dAgreeComments comment
    _ <-divClass "comment__right" $
      listWithKey dAgainstComments comment

    eNewComment <- commentEntry
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

footer :: MonadWidget t m => m ()
footer = do
  el "footer"  $ el "div" $ text "This is a new text line"
  pure ()

header :: MonadWidget t m => m ()
header = do
  el "header" navWidget
  pure ()

loading :: MonadWidget t m => m ()
loading = divClass "loading" $ text "loading..."

navWidget :: MonadWidget t m => m ()
navWidget =
  el "nav" $
    divClass "nav__content" $ text "吵架与看笑话"

topicInput :: MonadWidget t m => m (Event t Topic)
topicInput = do
  rec
      titleInput  <- textInput $ def & setValue .~ ("" <$ result)
      contentInput <- textInput $ def & setValue .~ ("" <$ result)
      submit <- button "Submit"
      dTopic <- Topic `mapDyn` constDyn Nothing `apDyn` value titleInput `apDyn` value contentInput `apDyn` constDyn Nothing
      let result = ffilter (\t -> (not . null . topicTitle) t && (not . null . topicContent) t ) $
              attachWith const (current dTopic) submit
  return result
