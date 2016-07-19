{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Reflex
import Reflex.Dom
import Data.Time
import Data.Map (Map)

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
