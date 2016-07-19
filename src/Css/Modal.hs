{-# LANGUAGE OverloadedStrings #-}
module Css.Modal (
  modal
) where

import Clay

modal :: Css
modal = do
  ".modal" ? do
    position fixed
    top nil
    right nil
    bottom nil
    left nil
    zIndex 1050
    overflow hidden

  ".modal-dialog" ? do
    position relative
    width $ px 600
    sym2 margin (px 30) auto
    {-transitions "transform" (sec 0.3) easeOut (sec 0)-}
  ".modal-content" ? do
    position relative
    backgroundColor "#fff"
    backgroundClip $ boxClip paddingBox
    border solid (px 1) (rgba 0 0 0 0.2)
    sym borderRadius (px 6)
    boxShadow nil (px 3) (px 9) (rgba 0 0 0 0.5)
  ".modal-header" ? do
    minHeight $ px 16.43
    sym padding (px 15)
    borderBottom solid (px 1) "#e5e5e5"
    color "#333"

  ".modal-body" ? do
    position relative
    sym padding (px 15)

  ".modal-footer" ? do
    sym padding (px 15)
    textAlign $ alignSide sideRight
    borderTop solid (px 1) "#e5e5e5"

  ".close" ? do
    float floatRight
    fontSize $ px 21
    fontWeight $ weight 700
    color black
    opacity 0.2
    textShadow nil (px 1) nil "#fff"
    cursor pointer




