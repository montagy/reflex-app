{-# LANGUAGE OverloadedStrings #-}
module Css.Button where

import Clay
import Prelude hiding (filter)

topicToggle :: Css
topicToggle =
  ".topic__toggle" ? do
    position fixed
    right $ px 50
    bottom $ px 200
    backgroundColor "#333"
    color "#ccc"
    width $ px 100
    height $ px 60
    borderWidth nil
    sym borderRadius (px 4)
    transitionDuration (sec 0.6)
    overflow hidden
    focus & outlineWidth nil
    before & do
      content (stringContent "")
      display block
      position absolute
      backgroundColor $ rgba 255 255 255 0.5
      width $ px 60
      height $ pct 100
      left nil
      top nil
      opacity 0.5
      filter $ blur (px 30)
      transforms [translateX (px $ -100), skewX (deg $ -30.0)]
    after & do
      content (stringContent "")
      display block
      position absolute
      backgroundColor $ rgba 255 255 255 0.2
      width $ px 30
      height $ pct 100
      left $ px 30
      top nil
      opacity 0
      filter $ blur (px 5)
      transforms [translateX (px $ -100), skewX (deg $ -15)]
    hover & do
      backgroundColor "#338033"
      cursor pointer

      before & do
        transforms [translateX (px 200), skewX (deg $ -15)]
        opacity 0.6
        transitionDuration $ sec 0.7
      after & do
        transforms [translateX (px 200), skewX (deg $ -15)]
        opacity 1
        transitionDuration $ sec 0.7

{-btn :: Css-}
{-btn = do-}
  {-".btn" ? do-}


