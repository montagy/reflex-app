{-# LANGUAGE CPP #-}
module Lib (
  app,
) where

{-import Reflex-}
import Reflex.Dom
{-import qualified Data.Map as M-}
{-import Reflex.Dom.Contrib.Router-}
import Control.Monad

import Css (bs)
import qualified Views.Home as Home

{-
 -router :: MonadWidget t m => String -> m (Event t String)
 -router "/" = home
 -router "/post" = post
 -router _ = home
 -}

app :: IO ()
#ifdef PRODUCT
app = mainWidget $ void Home.page
#else
app = mainWidgetWithCss bs $ void Home.page
#endif
