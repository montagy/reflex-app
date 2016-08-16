{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Views.Home (
  page
) where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import qualified Data.Map as Map
import Data.Time
import qualified Data.Text as T
import Data.Text (Text)

import Common.Types
import Api
import Data.Bson (timestamp, ObjectId)
import Widgets.Login
import Utils
import qualified Storage

page :: MonadWidget t m => m ()
page = do
  dUser <- header
  divClass "container" $ do
    rec
      dAttr <- holdDyn mempty $
        ("class" =: "loading" <>) . (\b -> if b then mempty else  displayNone)
          <$> leftmost [True <$ eItemClick, False <$ eTopic]
      loading dAttr
      eTopic <- leftmost <$> sequence [getNewestTopic, fetchTopic eItemClick, pure eNewTopic]
      --TODO 刷新按钮来获得随机或最新的topicList
      eTopicList <- leftmost <$> sequence [getTopicList, fetchTopicList eNewTopic]
      divClass "topic_wrapper" $ topicView dUser eTopic
      (eItemClick, eNewTopic) <- el "aside" $ do
        eObj <- divClass "topic__list" $ do
          el "header"  $ text "Hot Topic"
          elAttr "div" ("class" =: "list__content") $
            switchPromptlyDyn <$>  widgetHold (pure never) (topicList <$> eTopicList)

        eNewTopic' <- switchPromptly never =<< dyn (maybe (pure never) topicInput <$> dUser)
        pure (eObj, eNewTopic')

    pure ()
  {-footer-}

topicInput :: MonadWidget t m => Token -> m (Event t Topic)
topicInput _ = do
  eToggle <- buttonAttr "New" (constDyn $ "class" =: "topic__toggle")
  dIsToogled <- toggle False eToggle
  let dAttr = (("class" =: "topic__form" <>) . (\b -> if b then displayBlock else displayNone)) <$> dIsToogled
  elDynAttr "div" dAttr $ do
    el "header" $ text "Create a new topic"
    rec
      title <- divClass "form-group" $ textInput $ def & setValue .~ ("" <$ submit)
        & attributes .~ constDyn ("placeholder" =: "input a title" <> "class" =: "form-control")

      content <- divClass "form-group" $ textArea $ def & setValue .~ ("" <$ submit)
        & attributes .~ constDyn ("placeholder" =: "input content" <> "class" =: "form-control")

      submit <- divClass "form-group" $ buttonAttr "submit" $ constDyn ("class" =: "btn")
    let bTitle = current $ value title
        bContent = current $ value content
        f :: Text -> Text -> Topic
        f c t = def{topicTitle = t, topicContent = c}
        g :: Topic -> Bool
        g Topic{..} =
          let notnull = not . T.null
            in
          notnull topicTitle && notnull topicContent
        eSubmitTopic = ffilter g $ attachWith f bContent (tag bTitle submit)

    postTopic eSubmitTopic

--TODO return a selcted key Dynamic
topicList :: MonadWidget t m => [Topic] -> m (Event t ObjectId)
topicList ts = do
  let view :: (MonadWidget t m) => Topic -> m (Event t ObjectId)
      view t = do
        (dom, _) <- elAttr' "div" ("class" =: "xiaohua") $ text (topicTitle t)
        pure $ fmapMaybe id $ topicId t <$ domEvent Click dom

  es <- mapM view ts
  nubEvent (leftmost es)

topicView :: MonadWidget t m => Dynamic t (Maybe Token) -> Event t Topic -> m ()
topicView dUser eTopic = do
  dTopic <- holdDyn def eTopic
  divClass "topic" $ do
    divClass "topic__title" $ dynText $ topicTitle <$> dTopic
    divClass "topic__content" $ dynText $ topicContent <$> dTopic
  divClass "comment" $ do
    rec
      dCmts <- holdDyn [] $ leftmost [topicComments <$> eTopic, eCmts]
      commentsView dCmts

      eCmts <- switchPromptly never =<< dyn (maybe (pure never) (commentEntry dTopic) <$> dUser)
    pure ()
  pure ()

--TODO not load comment entry when topic is initialTopic
commentEntry :: MonadWidget t m => Dynamic t Topic -> Token ->  m (Event t [Comment])
commentEntry dTopic tok = divClass "comment_input" $ do
  rec
    let newComment :: Topic -> CommentSide -> Text -> Maybe Comment
        newComment t s c =
          case topicId t of
            Nothing -> Nothing
            Just id' -> case T.strip c of
                          "" -> Nothing
                          c' -> Just $ def{commentTopicId = Just id', commentSide = s, commentContent = c'}
        selectList = Agree =: "agree" <> Against =: "against"
        dNewComment = newComment <$>  dTopic <*> value drop <*> value area
        eNewComment = fmapMaybe id $ tag (current dNewComment) submit

    eComment <- postComment' tok eNewComment
    drop <- divClass "form-group" $ dropdown Agree (constDyn selectList) $ def
      & attributes .~ constDyn ("class" =: "form-control")
    area <- divClass "form-group" $ textArea $ def & setValue .~ ("" <$ submit) &
      attributes .~ constDyn (mconcat ["class" =: "form-control", "row" =: "3"])
    submit <- divClass "form-group" $ buttonAttr "submit" $ constDyn (mconcat ["type" =: "button", "class" =: "btn"])

  pure eComment

commentsView :: MonadWidget t m =>Dynamic t [Comment]-> m ()
commentsView dComments = do
  let comment :: MonadWidget t m => Int -> Dynamic t Comment -> m ()
      comment _ c = do
        let dContent = commentContent <$> c
            dName = (maybe "匿名:" (<> ":") . commentUser) <$> c
            dC = zipDynWith (<>) dName dContent
        divClass "comment__item" $ do
          divClass "comment__content" $ dynText dC
          zone <- liftIO getCurrentTimeZone
          let time = ffor c $ \cm ->
                case commentId cm of
                  Nothing -> "未提交状态"
                  Just oid -> "time:" <> prettyTime (utcToZonedTime zone $ timestamp oid)
          divClass "comment_time" $ dynText time

      dComments' = (Map.fromList . zip [1..]) <$> dComments
      dAgreeComments = Map.filter (\x -> commentSide x == Agree) <$> dComments'
      dAgainstComments = Map.filter (\x -> commentSide x == Against) <$> dComments'

  _ <- divClass "comment__left" $ do
    el "header" $ text "Agree"
    divClass "comment__container" $
      listWithKey dAgreeComments comment

  _ <-divClass "comment__right" $ do
    el "header" $ text "Against"
    divClass "comment__container" $
      listWithKey dAgainstComments comment

  pure ()

--footer :: MonadWidget t m => m ()
--footer =
  --el "footer" $ el "div" $ text "This is a new text line"

header :: MonadWidget t m => m (Dynamic t (Maybe Token))
header =
  el "header" navWidget

loading :: MonadWidget t m => Dynamic t AttributeMap -> m ()
loading dAttr = elDynAttr "div" dAttr $ text "loading..."

navWidget :: MonadWidget t m => m (Dynamic t (Maybe Token))
navWidget = do
  divClass "head__title" $ text "Argue Or Laugh"
  divClass "login-logout" $ do
    rec
      eInitToken <- confirmUser
      let eToken = leftmost [eInitToken, eToken']

      eToken' <- switchPromptlyDyn <$> widgetHold (pure never) (loginOrLogoutW <$> eToken)

    holdDyn Nothing eToken

loginOrLogoutW :: MonadWidget t m => Maybe Token -> m (Event t (Maybe Token))
loginOrLogoutW Nothing =  loginW
loginOrLogoutW (Just _) = do
  --TODO 用户功能,下拉菜单型
  eLogout <- buttonAttr "logout" (constDyn formControl)
  performEvent_ (Storage.remove "token" <$ eLogout)
  pure $ Nothing <$ eLogout
