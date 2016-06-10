{-# LANGUAGE OverloadedStrings #-}
module Css where

import Clay
import Data.ByteString
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

bs :: ByteString
bs = TE.encodeUtf8 . TL.toStrict . render $ css

css :: Css
css =
  ".navbar" ?
    backgroundColor (rgb 255 255 0)

