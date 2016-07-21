{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Widgets.Login (
  loginW,
  formControl
) where

import Reflex
import Reflex.Dom
import Common.Types
import qualified Api
import Utils
import Data.Monoid

formControl :: AttributeMap
formControl = "class" =: "form-control"

loginHeader :: MonadWidget t m => m (Event t ())
loginHeader =
  divClass "modal-header" $ do
    btn <- buttonAttr "X" $ constDyn ("class" =: "close")
    elAttr "h4" ("class" =: "modal-title") $ text "Modal title"
    pure btn

loginBody :: MonadWidget t m => m (Event t (Either String UserInfo))
loginBody = divClass "modal-body" $ do
  name <- textInput $ def & attributes .~ constDyn formControl
  pwd <- textInput $ def & attributes .~ constDyn formControl
  submit <- buttonAttr "Submit" $ constDyn formControl
  user <- combineDyn User (value name) (value pwd)
  Api.login (tagDyn user submit)

loginFooter :: MonadWidget t m => m (Event t (), Event t ())
loginFooter =
  divClass "modal-footer" $ do
    cancel <- buttonAttr "Cancel" $ constDyn formControl
    submit <- buttonAttr "OK" $ constDyn formControl
    pure (cancel, submit)

loginModal :: MonadWidget t m => Event t () -> m (Event t (Either String UserInfo), Dynamic t AttributeMap)
loginModal eToggle =
  divClass "modal-dialog" $ divClass "modal-content" $ do
    eClose <- loginHeader
    eeUserInfo <- loginBody
    (eCancel, eOk) <- loginFooter
    dModalAttr' <- holdDyn False $
      leftmost [True<$ eToggle, False<$ eClose, False <$ eCancel, False <$ eOk]
    dModalAttr <- mapDyn visiblility dModalAttr'
    pure (eeUserInfo, dModalAttr)

  where
    visiblility True = displayBlock
    visiblility False = displayNone

loginW :: MonadWidget t m => m (Event t (Maybe UserInfo))
loginW = do
  rec
    modal <- buttonAttr "登陆" dAttr
    dAttr <- mapDyn (either (const displayBlock) (const displayNone)) dUserInfo
    dModalAttr' <- mapDyn (("class" =: "modal") <>) dModalAttr
    (eeUserInfo, dModalAttr) <- elDynAttr "modal" dModalAttr' $
      loginModal modal
    dUserInfo <- holdDyn (Left "") eeUserInfo

  pure $ either (const Nothing) Just <$> eeUserInfo

