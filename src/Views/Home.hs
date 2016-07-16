{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Views.Home (
  page
) where

import Reflex
import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Data.Function (on)

import Common.Types
import Api
import Data.Bson (timestamp, ObjectId)

prettyTime :: FormatTime t => t -> String
prettyTime = formatTime defaultTimeLocale "%F %T"

nubEvent :: (MonadWidget t m, Eq a) => Event t a -> m (Event t a)
nubEvent e = do
  d <- holdDyn Nothing (Just <$> e)
  return $ fmapMaybe id $ updated (nubDyn d)

buttonAttr :: (MonadWidget t m) => Dynamic t (Map String String) -> m (Event t ())
buttonAttr attr = do
  (element, _) <- elDynAttr' "button" attr $ text "Submit"
  pure $ domEvent Click element

page :: MonadWidget t m => m ()
page = do
  header
  divClass "container" $ do
    rec
      dAttr <- holdDyn mempty $
        ("class" =: "loading" <>) . (\b -> if b then mempty else  "style" =: "display:none")
          <$> leftmost [True <$ eItemClick, False <$ eTopic]
      loading dAttr
      eTopic <- leftmost <$> sequence [getNewestTopic, fetchTopic eItemClick, pure eNewTopic]
      --TODO 刷新按钮来获得随机或最新的topicList
      eTopicList <- leftmost <$> sequence [getTopicList, fetchTopicList eNewTopic]
      divClass "topic_wrapper" $ topicView eTopic
      (eItemClick, eNewTopic) <- divClass "xiaohua_wrapper raiuds" $ do
        eObj <- divClass "topic__list" $ switchPromptlyDyn <$>  widgetHold (pure never) (topicList' <$> eTopicList)
        eNewTopic' <- divClass "topic__form" topicInput
        pure (eObj, eNewTopic')

    -- on fire , is login
    _ <- loginW
    pure ()
  footer

topicInput :: MonadWidget t m => m (Event t Topic)
topicInput = do
  rec
    title <- textInput $ def & setValue .~ ("" <$ submit)
      & attributes .~ constDyn ("placeholder" =: "input a title" <> "class" =: "form-control")

    content <- textArea $ def & setValue .~ ("" <$ submit)
      & attributes .~ constDyn ("placeholder" =: "input content" <> "class" =: "form-control")

    submit <- buttonAttr $ constDyn ("class" =: "form-control")
  let
      bTitle = current $ value title
      bContent = current $ value content
      f :: String -> String -> Topic
      f c t = Topic Nothing (T.pack t ) (T.pack c) []
      g :: Topic -> Bool
      g Topic{..} =
        let notnull = not . T.null
          in
        notnull topicTitle && notnull topicContent
      eSubmitTopic = ffilter g $ attachWith f bContent (tag bTitle submit)

  postTopic eSubmitTopic

--TODO return a selcted key Dynamic
topicList' :: MonadWidget t m => [Topic] -> m (Event t ObjectId)
topicList' ts = do
  let
      view :: (MonadWidget t m) => Topic -> m (Event t ObjectId)
      view t = do
        (dom, _) <- elAttr' "div" ("class" =: "xiaohua") $ text (T.unpack $ topicTitle t)
        pure $ fmapMaybe id $ topicId t <$ domEvent Click dom

  es <- mapM view ts
  nubEvent (leftmost es)

initialTopic :: Topic
initialTopic = Topic Nothing "" "" []

topicView :: MonadWidget t m => Event t Topic -> m ()
topicView eTopic = do
  dTopic <- holdDyn initialTopic eTopic
  divClass "topic radius" $ do
    divClass "topic__title" $ dynText =<< mapDyn (T.unpack . topicTitle) dTopic
    divClass "topic__content" $ dynText =<< mapDyn (T.unpack . topicContent) dTopic
  divClass "comment radius" $ do
    rec
      dCmts <- holdDyn [] $ leftmost [topicComments <$> eTopic, eCmts]
      commentsView dCmts

      eCmts <- commentEntry dTopic
    pure ()
  pure ()

--TODO not load comment entry when topic is initialTopic
commentEntry :: MonadWidget t m => Dynamic t Topic ->  m (Event t [Comment])
commentEntry dTopic = divClass "comment_input" $ do
  rec
    let
        newComment :: Topic -> CommentSide -> String -> Maybe Comment
        newComment t s c =
          case topicId t of
            Nothing -> Nothing
            Just id' -> case maybeStrip c of
                          Nothing -> Nothing
                          Just c' -> Just $ Comment Nothing id' s c'
        selectList = Agree =: "agree" <> Against =: "against"

    dNewComment <- newComment `mapDyn` dTopic `apDyn` value drop `apDyn` value area
    let eNewComment = fmapMaybe id $ tag (current dNewComment) submit
    eComment <- postComment' eNewComment

    drop <- dropdown Agree (constDyn selectList) $ def &
      attributes .~ constDyn ("class" =: "form-control")
    area <- textArea $ def & setValue .~ ("" <$ submit) &
      attributes .~ constDyn (mconcat ["class" =: "form-control", "row" =: "3"])
    submit <- buttonAttr  $ constDyn (mconcat ["type" =: "button", "class" =: "form-control"])

  return eComment

commentsView :: MonadWidget t m =>Dynamic t [Comment]-> m ()
commentsView dComments = do
  let
    comment :: MonadWidget t m => Int -> Dynamic t Comment -> m ()
    comment _ c = do
      content <- mapDyn (T.unpack . commentContent) c
      divClass "comment__item" $ do
        _ <- el "p" $ dynText content
        zone <- liftIO getCurrentTimeZone
        time <- forDyn c $ \cm -> case commentId cm of
                                  Nothing -> "未提交状态"
                                  Just oid -> "time:" ++ prettyTime (utcToZonedTime zone $ timestamp oid)

        _ <- el "p" $ dynText time
        pure ()

  dComments' <- mapDyn (Map.fromList . zip [1..]) dComments
  rec
    dAgreeComments <- mapDyn (Map.filter (\x -> commentSide x == Agree)) dComments'
    dAgainstComments <- mapDyn (Map.filter (\x -> commentSide x == Against)) dComments'
    _ <- divClass "comment__left" $
      listWithKey dAgreeComments comment
    _ <-divClass "comment__right" $
      listWithKey dAgainstComments comment

  pure ()

stripString :: String -> Text
stripString  = T.strip . T.pack

maybeStrip :: String -> Maybe Text
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

loading :: MonadWidget t m => Dynamic t (Map String String) -> m ()
loading dAttr = elDynAttr "div" dAttr $ text "loading..."

navWidget :: MonadWidget t m => m ()
navWidget =
  el "nav" $
    divClass "nav__content" $ text "吵架与看笑话"

loginW :: MonadWidget t m => m (Event t UserInfo)
loginW =
  divClass "login-form" $ do
    name <- textInput def
    pwd <- textInput def
    submit <- button "Submit"
    user <- combineDyn (User `on` T.pack) (value name) (value pwd)
    eeUserInfo <- login (tagDyn user submit)
    let
        eAuthErr = fmapMaybe (either Just (const Nothing)) eeUserInfo
        eAuthSuccess = fmapMaybe (either (const Nothing) Just) eeUserInfo

    _ <- dynText =<< holdDyn "" (leftmost [eAuthErr, T.unpack . infoName <$> eAuthSuccess])
    pure eAuthSuccess
