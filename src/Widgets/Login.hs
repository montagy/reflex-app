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
import Data.Monoid ((<>))
import Data.Function (on)
import qualified Data.Text as T
import System.Random
import Control.Monad.IO.Class (liftIO)
import Control.Monad

loginHeader :: MonadWidget t m => m (Event t ())
loginHeader =
  divClass "modal-header" $ do
    btn <- buttonAttr "X" $ constDyn ("class" =: "close")
    elAttr "h4" ("class" =: "modal-title") $ text "Input a user name"
    pure btn

{-
 -loginBody :: MonadWidget t m => m (Event t (Either String Token))
 -loginBody = divClass "modal-body" $ do
 -  name <- textInput $ def & attributes .~ constDyn formControl
 -  pwd <- textInput $ def & attributes .~ constDyn formControl
 -  submit <- buttonAttr "Submit" $ constDyn formControl
 -  user <- combineDyn (User `on` T.pack) (value name) (value pwd)
 -  Api.login (tagDyn user submit)
 -}
--another loginBody
onlyNeedName :: MonadWidget t m => m (Event t (Either String Token))
onlyNeedName = divClass "modal-body" $ do
  rec
    elDynAttr "label" (constDyn $ "class" =: "login_info") $ dynText dErrMsg
    name <- divClass "form-group" $ textInput $ def & attributes .~ constDyn ("class" =: "form-control")
    submit <- divClass "form-group" $ buttonAttr "Submit" $ constDyn ("class" =: "btn")
    pwd <- liftIO $ replicateM 6 $ randomRIO ('1', '9')

    let eName = tagDyn (value name) submit
        eRightName = ffilter (\s -> length s > 4) eName
        eUser = (\n -> on User T.pack n pwd) <$> eRightName
        eErr = either id (const "") <$> result

    dErrMsg <- holdDyn "" $ (<> " length less than 4") <$> ffilter (\s -> length s <= 4) (leftmost [eName, eErr])
    result <- Api.login eUser
  pure result

loginFooter :: MonadWidget t m => m (Event t (), Event t ())
loginFooter =
  divClass "modal-footer form-group" $ do
    cancel <- buttonAttr "Cancel" $ constDyn formControl
    submit <- buttonAttr "OK" $ constDyn formControl
    pure (cancel, submit)

loginModal :: MonadWidget t m => Event t () -> m (Event t (Either String Token), Dynamic t AttributeMap)
loginModal eToggle =
  divClass "modal-dialog" $ divClass "modal-content" $ do
    eClose <- loginHeader
    --eeUserInfo <- loginBody
    eeUserInfo <- onlyNeedName
    (eCancel, eOk) <- loginFooter
    dModalAttr' <- holdDyn False $
      leftmost [True<$ eToggle, False<$ eClose, False <$ eCancel, False <$ eOk]
    dModalAttr <- mapDyn visiblility dModalAttr'
    pure (eeUserInfo, dModalAttr)

  where
    visiblility True = displayBlock
    visiblility False = displayNone

loginW :: MonadWidget t m => m (Event t (Maybe Token))
loginW = do
  rec
    modal <- buttonAttr "Login" dAttr
    dAttr <- mapDyn (either (const $ displayBlock <> formControl) (const $ displayNone <> formControl)) dUserInfo
    dModalAttr' <- mapDyn (("class" =: "modal") <>) dModalAttr
    (eeUserInfo, dModalAttr) <- elDynAttr "modal" dModalAttr' $
      loginModal modal
    dUserInfo <- holdDyn (Left "") eeUserInfo

  pure $ either (const Nothing) Just <$> eeUserInfo

