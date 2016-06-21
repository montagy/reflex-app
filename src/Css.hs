{-# LANGUAGE OverloadedStrings #-}
module Css where

import Prelude hiding (div, rem)
import Clay
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Data.Monoid ((<>))

bs :: ByteString
bs = TE.encodeUtf8 . TL.toStrict . render $ css

css :: Css
css = do
  reset
  body ? do
    backgroundColor "#e9e9e9"
    fontFamily [] [monospace]
    fontSize (em 1.5)
    paddingTop $ px 62
  header ? do
    let headerHeight = px 42
    width (pct 100)
    position fixed
    marginBottom $ px 20
    top $ px 0
    left $ px 0
    right $ px 0
    height headerHeight
    zIndex 999
    boxShadow nil (px 1) nil (rgba 0 0 0 64)
    backgroundColor "#2b5166"
    color white
    nav ? do
      div ? lineHeight headerHeight

  ".container" ? do
    margin nil auto nil auto
    width (pct 100)
    maxWidth (px 960)
    ".topic_wrapper" ? do
      width $ px 590
      float floatLeft

    ".xiaohua_wrapper" ? do
      width $ px 360
      float floatRight
      textAlign (alignSide sideCenter)
      lineHeight $ rem 2
      ".xiaohua" ? do
        boxShadow nil (px 2) (px 5) (rgba 0 0 0 51)
        padding (px 10) (px 10) (px 10) (px 10)
        backgroundColor white

  "div.topic" ? do
    width $ pct 100
    padding (px 10) (px 16) (px 25) (px 16)
    backgroundColor white
    "div.topic__title" ? do
      fontSize $ rem 1.5
      marginTop $ px 5
      paddingBottom $ px 10
      borderBottom solid (px 1) "#e2e2e2"
      textAlign (alignSide sideCenter)

    "div.topic__content" ? do
      marginTop $ px 15
      lineHeight $ rem 1.5
  "div.comment" ? do
    marginTop $ px 20
    paddingTop $ px 10
    lineHeight $ rem 2
    div <? do
      padding (px 20) (px 16) (px 10) (px 16)
      backgroundColor white
    ".comment__item" ? do
      padding (px 5) (px 4) (px 5) (px 4)
      borderTop solid (px 1) "#f2f2f2"

    ".comment__left" ? do
      float floatLeft
      width $ px 290

    ".comment__right" ? do
      float floatRight
      width $ px 290

    ".comment__input" ? do
      marginTop $ px 20

  ".clear" ? clear both
  ".radius" ? borderRadius (px 5) (px 5) (px 5) (px 5)
  footer ? do
    width $ pct 100
    position fixed
    bottom $ px 0
    backgroundColor blue
    div <? lineHeight (rem 2)


reset :: Css
reset = do
  body ? do
    margin nil nil nil nil
    padding nil nil nil nil
    color "#333"

  body <> div <> nav <> header ?
    boxSizing borderBox

  select ? do
    margin (px 5) (px 5) (px 5) (px 5)
    lineHeight $ rem 1.6


    {-ul ? do-}
      {-borderBottom solid (px 1) (parse "#d3d3d3")-}
      {-li ? do-}
        {-display inlineBlock-}
        {-width (pct 25)-}
        {-padding (px 10) 0 (px 10) 0-}
