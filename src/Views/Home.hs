{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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
import Storage (remove)

page :: MonadWidget t m => m ()
page = do
  dMUser <- header
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

    pure ()
  footer

topicInput :: MonadWidget t m => m (Event t Topic)
topicInput = do
  rec
    title <- textInput $ def & setValue .~ ("" <$ submit)
      & attributes .~ constDyn ("placeholder" =: "input a title" <> "class" =: "form-control")

    content <- textArea $ def & setValue .~ ("" <$ submit)
      & attributes .~ constDyn ("placeholder" =: "input content" <> "class" =: "form-control")

    submit <- buttonAttr "submit" $ constDyn ("class" =: "form-control")
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

topicView :: MonadWidget t m => Event t Topic -> m ()
topicView eTopic = do
  dTopic <- holdDyn def eTopic
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
    submit <- buttonAttr "submit" $ constDyn (mconcat ["type" =: "button", "class" =: "form-control"])

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

footer :: MonadWidget t m => m ()
footer = do
  el "footer"  $ el "div" $ text "This is a new text line"
  pure ()

header :: MonadWidget t m => m (Dynamic t (Maybe UserInfo))
header =
  el "header" navWidget

loading :: MonadWidget t m => Dynamic t AttributeMap -> m ()
loading dAttr = elDynAttr "div" dAttr $ text "loading..."

navWidget :: MonadWidget t m => m (Dynamic t (Maybe UserInfo))
navWidget =
  el "nav" $
    divClass "nav__content" $ do
      text "吵架与看笑话"
      divClass "login-logout" $ do
        rec
          eInitUser <- confirmUser
          let eUserInfo = leftmost [eInitUser, eUserInfo']
          eUserInfo' <- switchPromptlyDyn <$> widgetHold (return never) (userHelper <$> eUserInfo)

        holdDyn Nothing eUserInfo

userHelper :: MonadWidget t m => Maybe UserInfo -> m (Event t (Maybe UserInfo))
userHelper Nothing =  loginW
userHelper (Just _) = do
  eLogout <- button "logout"
  performEvent_ (remove "user" <$ eLogout)
  pure $ Nothing <$ eLogout
