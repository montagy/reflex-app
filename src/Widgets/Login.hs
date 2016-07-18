{-# LANGUAGE OverloadedStrings #-}
module Widgets.Login (
  loginModal
) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Modal
import Common.Types
import qualified Api as A

loginBody :: MonadWidget t m => m (Dynamic t (Either String UserInfo))
loginBody = do
  name <- textInput def
  pwd <- textInput def
  submit <- button "Submit"
  user <- combineDyn User (value name) (value pwd)
  holdDyn (Left "") =<< A.login (tagDyn user submit)

login :: MonadWidget t m => m (Event t (Either String UserInfo), Event t ())
login =
  mkModalBody (button "close") loginFooter loginBody

loginFooter :: MonadWidget t m => Dynamic t (Either String UserInfo) -> m (Event t (), Event t ())
loginFooter body =
  divClass "modal-footer" $ do
    cancel <- button "Cancel"
    submit <- button "OK"
    pure (cancel, submit)

loginModal :: MonadWidget t m => m (Dynamic t (Either String UserInfo))
loginModal = do
  modal <- button "登陆"
  dShow <- toggle False modal
  eeUserInfo <- hidingModal DisplayNone (ModalConfig $ "class" =: "modal") (updated dShow) login
  holdDyn (Left "") eeUserInfo
