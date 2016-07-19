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

formControl :: Reflex t => Dynamic t AttributeMap
formControl = constDyn ("class" =: "form-control")

loginHeader :: MonadWidget t m => m (Event t ())
loginHeader =
  divClass "modal-header" $ do
    btn <- buttonAttr "X" $ constDyn ("class" =: "close")
    elAttr "h4" ("class" =: "modal-title") $ text "Modal title"
    pure btn

loginBody :: MonadWidget t m => m (Dynamic t (Either String UserInfo))
loginBody = do
  name <- textInput $ def & attributes .~ formControl
  pwd <- textInput $ def & attributes .~ formControl
  submit <- buttonAttr "Submit" formControl
  user <- combineDyn User (value name) (value pwd)
  holdDyn (Left "") =<< Api.login (tagDyn user submit)

loginFooter :: MonadWidget t m => Dynamic t (Either String UserInfo) -> m (Event t (), Event t ())
loginFooter body =
  divClass "modal-footer" $ do
    cancel <- buttonAttr "Cancel" formControl
    submit <- buttonAttr "OK" formControl
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

