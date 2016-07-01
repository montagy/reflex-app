{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Views.Home (
  page
) where

import Reflex
import Reflex.Dom
{-import Control.Monad-}
import Data.Monoid
{-import Data.Map (Map)-}
import qualified Data.Map as Map
import Data.Time
{-import Data.Maybe-}

import qualified Data.JSString as S
import Common.Types
import Api
import Data.Bson (timestamp)

--TODO 东八区时间
z8Time :: FormatTime t => t -> String
z8Time = formatTime defaultTimeLocale "%F %T %Z"

nubEvent :: (MonadWidget t m, Eq a) => Event t a -> m (Event t a)
nubEvent e = do
  d <- holdDyn Nothing (Just <$> e)
  return $ fmapMaybe id $ updated (nubDyn d)

page :: MonadWidget t m => m ()
page = do
  header
  divClass "container" $ do
    rec
      dAttr <- holdDyn mempty $
        ("class" =: "loading" <>) . (\b -> if b then mempty else  "style" =: "display:none")
          <$> leftmost [True <$ eItemClick, False <$ eTopic]
      elDynAttr "div" dAttr $ text "loading"
      deTopic' <- widgetHold (fakeGetData "inital") (fakeGetData . show <$> eItemClick)
      let eTopic = switchPromptlyDyn deTopic'
      --TODO 刷新按钮来获得随机或最新的topicList
      eTopicList <- getTopicList
      divClass "topic_wrapper" $ topicView eTopic
      eItemClick <- divClass "xiaohua_wrapper raiuds" $ topicList eTopicList
    pure ()
  footer
--TODO return a selcted key Dynamic
topicList :: MonadWidget t m =>Event t [Topic] -> m (Event t Int)
topicList eTs = do
  let
      view :: (MonadWidget t m ) => Int -> Dynamic t Topic -> m (Event t Int)
      view k dT = do
        (dom, _) <- elAttr' "div" ("class" =: "xiaohua") $ dynText =<< mapDyn topicTitle dT
        pure $ k <$ domEvent Click dom

  dTs <- holdDyn Map.empty $  Map.fromList . zip [1..] <$> eTs
  selectEntry <- listWithKey dTs view
  eResult <- switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry
  nubEvent eResult

initialTopic :: Topic
initialTopic = Topic Nothing "" "" Nothing

topicView :: MonadWidget t m => Event t Topic -> m ()
topicView eTopic = do
  dTopic <- holdDyn initialTopic eTopic
  divClass "topic radius" $ do
    divClass "topic__title" $ dynText =<< mapDyn topicTitle dTopic
    divClass "topic__content" $ dynText =<< mapDyn topicContent dTopic

  rec
    divClass "comment radius" $ do
      dTopicCmts <- mapDyn topicComments dTopic
      let
          fun :: Maybe [Comment] -> Maybe Comment -> Maybe [Comment]
          fun Nothing Nothing = Nothing
          fun Nothing (Just m) = Just [m]
          fun mt Nothing = mt
          fun (Just ts) (Just t) = Just (ts ++ [t])

      dComts <- combineDyn fun dTopicCmts =<< holdDyn Nothing (Just <$> eEntryCmt)
      commentsView dComts

    eEntryCmt <- commentEntry dTopic
  pure ()

commentEntry :: MonadWidget t m => Dynamic t Topic ->  m (Event t Comment)
commentEntry dTopic = divClass "comment_input" $ do
  rec
    let
        selectList = Agree =: "agree" <> Against =: "against"
        eContent = fmapMaybe maybeStrip $ tag (current $ _textArea_value area) eSubmit
        dSide = _dropdown_value drop
        --before post
        eNewComment' = attachDynWith (Comment Nothing "") dSide eContent
        eNewComment = attachDynWith (\topic c -> c{commentTopic = topicTitle topic}) dTopic eNewComment'


    --after post
    eComment <- switchPromptlyDyn <$> widgetHold (pure eNewComment) (postComment <$> eNewComment)
    drop <- dropdown Agree (constDyn selectList) $ def &
      attributes .~ constDyn ("class" =: "form-control")
    area <- textArea $ def & setValue .~ ("" <$ eSubmit) &
      attributes .~ constDyn (mconcat ["class" =: "form-control",
                                      "row" =: "3"])
    eSubmit <- do
      (e, _) <- elAttr' "button" (mconcat ["type" =: "button", "class" =: "form-control"]) $ text "Submit"
      pure $ domEvent Click e
  {-return eNewComment-}
  pure $ leftmost [eNewComment, eComment]

commentsView :: MonadWidget t m =>Dynamic t (Maybe [Comment])-> m ()
commentsView dmComments = do
  let
    comment :: MonadWidget t m => Int -> Dynamic t Comment -> m ()
    comment _ c = do
      content <- mapDyn commentContent c
      divClass "comment__item" $ do
        dynText content
        time <- forDyn c $ \cm -> case commentId cm of
                                  Nothing -> "未提交状态"
                                  Just oid -> "time:" ++ z8Time (timestamp oid)

        dynText time

      pure ()

  dComments <- forDyn dmComments $
    \case
        Nothing -> Map.empty
        Just comments -> Map.fromList $ zip [1..] comments
  rec
    dAgreeComments <- mapDyn (Map.filter (\x -> commentSide x == Agree)) dComments
    dAgainstComments <- mapDyn (Map.filter (\x -> commentSide x == Against)) dComments
    _ <- divClass "comment__left" $
      listWithKey dAgreeComments comment
    _ <-divClass "comment__right" $
      listWithKey dAgainstComments comment

  pure ()

stripString :: String -> String
stripString  = S.unpack . S.strip . S.pack

maybeStrip :: String -> Maybe String
maybeStrip (stripString -> "") = Nothing
maybeStrip (stripString -> trimmed) = Just trimmed
-- | Add a new value to a map; automatically choose an unused key
{-insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v-}
{-insertNew_ v m = case Map.maxViewWithKey m of-}
  {-Nothing -> Map.singleton (toEnum 0) v-}
  {-Just ((k, _), _) -> Map.insert (succ k) v m-}

footer :: MonadWidget t m => m ()
footer = do
  el "footer"  $ el "div" $ text "This is a new text line"
  pure ()

header :: MonadWidget t m => m ()
header = do
  el "header" navWidget
  pure ()

{-loading :: MonadWidget t m => m ()-}
{-loading = divClass "loading" $ text "loading..."-}

navWidget :: MonadWidget t m => m ()
navWidget =
  el "nav" $
    divClass "nav__content" $ text "吵架与看笑话"

{-topicInput :: MonadWidget t m => m (Event t Topic)-}
{-topicInput = do-}
  {-rec-}
      {-titleInput  <- textInput $ def & setValue .~ ("" <$ result)-}
      {-contentInput <- textInput $ def & setValue .~ ("" <$ result)-}
      {-submit <- button "Submit"-}
      {-dTopic <- Topic `mapDyn` constDyn Nothing `apDyn` value titleInput `apDyn` value contentInput `apDyn` constDyn Nothing-}
      {-let result = ffilter (\t -> (not . null . topicTitle) t && (not . null . topicContent) t ) $-}
              {-attachWith const (current dTopic) submit-}
  {-return result-}
