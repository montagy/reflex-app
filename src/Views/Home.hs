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
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe

import Common.Types
import Api
import Data.Bson (timestamp, ObjectId)

--本地时间
z8Time :: FormatTime t => t -> String
z8Time = formatTime defaultTimeLocale "%F %T"

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
      eTopic' <- fakeGetData Nothing
      eTopic'' <- fetchTopic eItemClick
      let eTopic = leftmost [eTopic', eTopic'']
      --TODO 刷新按钮来获得随机或最新的topicList
      eTopicList <- getTopicList
      divClass "topic_wrapper" $ topicView eTopic
      eItemClick <- divClass "xiaohua_wrapper raiuds" $ do
        ee <- widgetHold (pure never) (topicList' <$> eTopicList)
        pure $ switchPromptlyDyn ee
    pure ()
  footer

topicList' :: MonadWidget t m => [Topic] -> m (Event t ObjectId)
topicList' ts = do
  let
      view :: (MonadWidget t m) => Topic -> m (Event t ObjectId)
      view t = do
        (dom, _) <- elAttr' "div" ("class" =: "xiaohua") $ text (T.unpack $ topicTitle t)
        pure $ fmapMaybe id $ topicId t <$ domEvent Click dom

  es <- mapM view ts
  nubEvent (leftmost es)


--TODO return a selcted key Dynamic
topicList :: MonadWidget t m =>Event t [Topic] -> m (Event t (Maybe ObjectId))
topicList eTs = do
  let
      view :: (MonadWidget t m ) => Int -> Dynamic t Topic -> m (Event t (Maybe ObjectId))
      view _ dT = do
        (dom, _) <- elAttr' "div" ("class" =: "xiaohua") $ dynText =<< mapDyn (T.unpack . topicTitle) dT
        pure $ attachDynWith (\d _ -> topicId d) dT (domEvent Click dom)

  dTs <- holdDyn Map.empty $  Map.fromList . zip [1..] <$> eTs
  selectEntry <- listWithKey dTs view
  eResult <- switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry
  nubEvent eResult

initialTopic :: Topic
initialTopic = Topic Nothing "" "" []

topicView :: MonadWidget t m => Event t Topic -> m ()
topicView eTopic = do
  dTopic <- holdDyn initialTopic eTopic
  divClass "topic radius" $ do
    divClass "topic__title" $ dynText =<< mapDyn (T.unpack . topicTitle) dTopic
    divClass "topic__content" $ dynText =<< mapDyn (T.unpack . topicContent) dTopic
--TODO重新设计
  rec
    divClass "comment radius" $ do
      dTopicCmts <- mapDyn topicComments dTopic
      commentsView dTopicCmts
      {-let-}
          {-fun :: Maybe [Comment] -> [Comment] -> Maybe [Comment]-}
          {-fun mc c = mc <> Just c-}
          {-eCmt = fmapMaybe id-}
            {-(attachDynWith (\topic cmt -> if topicTitle topic == commentTopic cmt then Just cmt else Nothing) dTopic eEntryCmt)-}

      {-
       -dComts <- combineDyn fun dTopicCmts =<<
       -  foldDyn (\cmt l -> l ++ [cmt]) [] eCmt
       -}
        {-foldDyn (<>) Nothing-}
            {-(attachDynWith (\topic cmt -> if topicTitle topic == commentTopic cmt then Just cmt else Nothing) dTopic eEntryCmt)-}
      {-commentsView dComts-}

    eEntryCmt <- commentEntry dTopic
  pure ()

-- not load comment entry when topic is initialTopic
commentEntry :: MonadWidget t m => Dynamic t Topic ->  m (Event t [Comment])
commentEntry dTopic = divClass "comment_input" $ do
  rec
    let
        newComment :: Topic -> CommentSide -> Text -> Maybe Comment
        newComment t s c =
          case topicId t of
            Nothing -> Nothing
            Just id' -> Just $ Comment Nothing id' s c
        selectList = Agree =: "agree" <> Against =: "against"
        eContent = fmapMaybe maybeStrip $ tag (current $ _textArea_value area) eSubmit
        dSide = _dropdown_value drop

        --before post
    dContent <- holdDyn "" eContent
    dNewComment <- newComment `mapDyn` dTopic `apDyn` dSide `apDyn` dContent
    let eNewComment = fmapMaybe id $ tagDyn dNewComment eSubmit
    eComment <- switchPromptlyDyn <$> widgetHold (pure never) (postComment <$> eNewComment)
    --after post
    --dComment <- dyn =<< mapDyn postComment dNewComment
    --eComment <- switchPromptly never dComment
    drop <- dropdown Agree (constDyn selectList) $ def &
      attributes .~ constDyn ("class" =: "form-control")
    area <- textArea $ def & setValue .~ ("" <$ eSubmit) &
      attributes .~ constDyn (mconcat ["class" =: "form-control",
                                      "row" =: "3"])
    eSubmit <- do
      (e, _) <- elAttr' "button" (mconcat ["type" =: "button", "class" =: "form-control"]) $ text "Submit"
      pure $ domEvent Click e

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
                                  Just oid -> "time:" ++ z8Time (utcToZonedTime zone $ timestamp oid)

        _ <- el "p" $ dynText time
        pure ()

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
