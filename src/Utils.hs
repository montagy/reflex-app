{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Reflex
import Reflex.Dom
import Data.Time
import qualified Data.Text as T
import Data.Text (Text)

prettyTime :: FormatTime t => t -> Text
prettyTime = T.pack . formatTime defaultTimeLocale "%F %T"

nubEvent :: (MonadWidget t m, Eq a) => Event t a -> m (Event t a)
nubEvent e = do
  d <- holdDyn Nothing (Just <$> e)
  return $ fmapMaybe id $ updated (uniqDyn d)

buttonAttr :: (MonadWidget t m) => Text -> Dynamic t AttributeMap -> m (Event t ())
buttonAttr t attr = do
  (ele, _) <- elDynAttr' "button" attr $ text t
  pure $ domEvent Click ele

displayNone, displayBlock :: AttributeMap
displayNone = "style" =: "display:none;"
displayBlock = "style" =: "display:block;"

formControl :: AttributeMap
formControl = "class" =: "btn"

-- | Add a new value to a map; automatically choose an unused key
{-insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v-}
{-insertNew_ v m = case Map.maxViewWithKey m of-}
  {-Nothing -> Map.singleton (toEnum 0) v-}
  {-Just ((k, _), _) -> Map.insert (succ k) v m-}
