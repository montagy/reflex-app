{-# LANGUAGE OverloadedStrings #-}
module Css where

import Prelude hiding (div)
import Clay
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

bs :: ByteString
bs = TE.encodeUtf8 . TL.toStrict . render $ css

css :: Css
css = do
  reset
  body ? do
    backgroundColor (hsl 20 7 82 )
    textAlign (alignSide sideCenter)
    fontFamily [] [monospace]
    fontSize (em 1.5)
  header ? do
    width (pct 100)
  nav ? do
    backgroundColor (hsl 60 29 41)
    maxWidth (px 960)
    margin nil auto nil auto
    position relative
    ul ? do
      borderBottom solid (px 1) (parse "#d3d3d3")
      li ? do
        display inlineBlock
        width (pct 25)
        padding (px 10) 0 (px 10) 0

  "div.content" ? do
    margin (px 50) auto nil auto
    width (pct 100)
    maxWidth (px 960)

reset :: Css
reset =
  star ? do
    margin nil nil nil nil
    padding nil nil nil nil
    boxSizing borderBox
