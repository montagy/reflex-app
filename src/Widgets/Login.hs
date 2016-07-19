{-# LANGUAGE OverloadedStrings #-}
module Widgets.Login (
  loginW
) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Modal
import Common.Types
import qualified Api
import Utils

loginHeader :: MonadWidget t m => m (Event t ())
loginHeader =
  divClass "modal-header" $ do
    elAttr "h4" ("class" =: "modal-title") $ text "Modal title"
    buttonAttr "X" $ constDyn ("class" =: "close")

loginBody :: MonadWidget t m => m (Dynamic t (Either String UserInfo))
loginBody = do
  name <- textInput $ def & attributes .~ constDyn ("class" =: "form-control")
  pwd <- textInput $ def & attributes .~ constDyn ("class" =: "form-control")
  submit <- button "Submit"
  user <- combineDyn User (value name) (value pwd)
  holdDyn (Left "") =<< Api.login (tagDyn user submit)

loginFooter :: MonadWidget t m => Dynamic t (Either String UserInfo) -> m (Event t (), Event t ())
loginFooter body =
  divClass "modal-footer" $ do
    cancel <- button "Cancel"
    submit <- button "OK"
    pure (cancel, submit)

loginModal :: MonadWidget t m => m (Event t (Either String UserInfo), Event t ())
loginModal =
  mkModalBody loginHeader loginFooter loginBody

loginW :: MonadWidget t m => m (Dynamic t (Either String UserInfo))
loginW = do
  modal <- button "登陆"
  dShow <- toggle False modal
  eeUserInfo <- hidingModal DisplayNone (ModalConfig $ "class" =: "modal") (updated dShow) loginModal
  holdDyn (Left "") eeUserInfo

