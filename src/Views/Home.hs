{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Views.Home (
  page
) where


import Reflex
import Reflex.Dom
import Control.Monad

import qualified Data.Text as T
import Common.Types
import Api (fakeGetBlog)

page :: forall t m .MonadWidget t m => m ()
page =
  elClass "div" "main" $ do
    el  "header" navWidget
    eArts :: Event t Article<- fakeGetBlog
    dView <- holdDyn loading $ article <$> eArts
    void $ dyn dView
    {-elClass "div" "content"  $ elClass "ul" "blogs" $ articleInfoListV eArts-}
    {-pure never-}


navWidget :: MonadWidget t m => m ()
navWidget =
  el "nav"  $
    elClass "ul" "list" $ do
      el "li" $ elAttr "a" ("href" =: "/") $ text "Home"
      el "li" $ elAttr "a" ("href" =: "/post") $ text "Post"

loading :: MonadWidget t m => m ()
loading = divClass "loading" $ text "loading..."

article :: forall t m. MonadWidget t m => Article -> m ()
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

