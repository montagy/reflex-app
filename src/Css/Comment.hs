{-# LANGUAGE OverloadedStrings #-}

module Css.Comment where

import Clay
import Prelude hiding (rem, div)
import qualified Clay.Flexbox as F
comment :: Css
comment = do
  ".comment" ? do
    marginTop $ px 20
    paddingTop $ px 10
    --lineHeight $ unitless 2
    display flex
    flexFlow row F.wrap
    justifyContent spaceBetween
    div <? do
      padding (px 20) (px 16) (px 10) (px 16)
      backgroundColor white

  ".comment__item" ? do
    sym2 padding (px 5) (px 4)
    borderBottom solid (px 1) $ rgb 217 217 217
    {-".coment__name" ? do-}
    {-".comment__content" ? -}
    {-".comment__time" ? undefined-}


  ".comment__left" ? width (pct 49)
  ".comment__right" ? width (pct 49)

  ".comment_input" ? do
    width $ pct 100
    marginTop $ px 20
