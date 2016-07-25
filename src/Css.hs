{-# LANGUAGE OverloadedStrings #-}
module Css where

import Prelude hiding (div, rem)
import Clay
import Clay.Selector (text)
import Data.ByteString (ByteString)
import qualified Clay.Flexbox as F
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Data.Monoid ((<>))
import Css.Modal (modal)

bs :: ByteString
bs = TE.encodeUtf8 . TL.toStrict . render $ css

headerHeight, footerHeight :: Double
headerHeight = 2.5
footerHeight = 2.2

fixedTop :: Css
fixedTop = do
  top nil
  left nil
  right nil
  zIndex 999
fixedBottom :: Css
fixedBottom = do
  bottom nil
  left nil
  right nil
  zIndex 998
selectedText :: Css
selectedText =
  text "::selection" ? do
    backgroundColor "#cce2ff"
    color $ rgba 0 0 0 0.86
css :: Css
css = do
  reset
  modal
  html ? fontSize (px 10)
  body ? do
    backgroundColor "#e9e9e9"
    fontFamily [] [monospace]
    paddingTop $ em headerHeight
    paddingBottom $ em footerHeight
    --font
    fontSize $ px 14
    lineHeight $ em $ 20.0 / 14.0
    header <? do
      position fixed
      fixedTop
      backgroundColor "#2b5166"
      --配色
      color white

    footer <? do
      backgroundColor white
      marginTop $ px 100
      sym2 padding (px 40) nil


  header ? do
    width (pct 100)
    {-marginBottom $ px headerBottom-}
    boxShadow nil (px 1) nil (rgba 0 0 0 64)
    -- flex
    display flex
    flexFlow row F.nowrap
    justifyContent spaceAround
    -- fontSize
    fontSize $ em 2
    lineHeight $ em 1.4285
  footer ? do
    width $ pct 100
    fontSize $ em $ footerHeight / 2
    lineHeight $ em footerHeight
    borderTop solid (px 1) "#e5e5e5"
    {-div <? lineHeight (rem 2)-}

  selectedText
  ".loading" ? do
    position absolute
    top $ px 50
    width $ pct 100
    textAlign (alignSide sideCenter)
  ".container" ? do
    margin (em 1) auto nil auto
    width (pct 100)
    maxWidth $ pct 80
    display flex
    flexFlow row F.nowrap
    justifyContent spaceBetween
    ".topic_wrapper" ? width (pct 66)

    ".xiaohua_wrapper" ? do
      width $ pct 33
      textAlign (alignSide sideCenter)
      ".xiaohua" ? do
        boxShadow nil (px 2) (px 5) (rgba 0 0 0 51)
        sym padding (px 10)
        backgroundColor white
        cursor pointer
        hover & backgroundColor blue


  ".topic" ? do
    width $ pct 100
    padding (px 10) (px 16) (px 25) (px 16)
    backgroundColor white
    ".topic__title" ? do
      fontSize $ rem 1.5
      marginTop $ px 5
      paddingBottom $ px 10
      borderBottom solid (px 1) "#e2e2e2"
      textAlign (alignSide sideCenter)

    ".topic__content" ? do
      marginTop $ px 15
      lineHeight $ rem 1.5
  ".comment" ? do
    marginTop $ px 20
    paddingTop $ px 10
    lineHeight $ rem 2
    display flex
    flexFlow row F.wrap
    justifyContent spaceBetween
    div <? do
      padding (px 20) (px 16) (px 10) (px 16)
      backgroundColor white
    ".comment__item" ? do
      sym2 padding (px 5) (px 4)
      borderTop solid (px 1) "#f2f2f2"

    ".comment__left" ? width (pct 49)
    ".comment__right" ? width (pct 49)

  ".comment_input" ? do
    width $ pct 100
    marginTop $ px 20

  ".radius" ? sym borderRadius (px 5)
  textarea # ".form-control" ? height auto
  ".form-control" ? do
    lineHeight $ em 2
    display block
    height $ px 34
    sym2 padding (px 6) (px 12)
    color "#555"
    backgroundColor "#fff"
    border solid (px 1) "#ccc"
    sym borderRadius (px 4)
    insetBoxShadow inset nil (px 1) (px 1) (rgba 0 0 0 19)
    transitions [("border-color", sec 0.15, easeInOut, sec 0),
                ("box-shadow", sec 0.15, easeInOut, sec 0)]

  {-header ?-}

reset :: Css
reset = do
  html <> body ? do
    sym margin nil
    sym padding nil
    color "#333"

  star ? boxSizing borderBox

  {-star # before <> star # after ? boxSizing borderBox-}

