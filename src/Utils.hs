{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Utils where

import Reflex
import Reflex.Dom
import Data.Time
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)

prettyTime :: FormatTime t => t -> String
prettyTime = formatTime defaultTimeLocale "%F %T"

nubEvent :: (MonadWidget t m, Eq a) => Event t a -> m (Event t a)
nubEvent e = do
  d <- holdDyn Nothing (Just <$> e)
  return $ fmapMaybe id $ updated (nubDyn d)

buttonAttr :: (MonadWidget t m) => String -> Dynamic t (Map String String) -> m (Event t ())
buttonAttr t attr = do
  (element, _) <- elDynAttr' "button" attr $ text t
  pure $ domEvent Click element

displayNone, displayBlock :: AttributeMap
displayNone = "style" =: "display:none;"
displayBlock = "style" =: "display:block;"

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
