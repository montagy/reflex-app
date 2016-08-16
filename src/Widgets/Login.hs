{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Widgets.Login (
  loginW,
) where

import Reflex
import Reflex.Dom
import Common.Types
import qualified Api
import Utils
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import System.Random
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Either (isRight)

loginHeader :: MonadWidget t m => m (Event t ())
loginHeader =
  divClass "modal-header" $ do
    btn <- buttonAttr "X" $ constDyn ("class" =: "close")
    elAttr "h4" ("class" =: "modal-title") $ text "Input a user name"
    pure btn

loginBody :: MonadWidget t m => m (Event t (Either Text Token))
loginBody = divClass "modal-body" $ do
  rec
    elDynAttr "label" (constDyn $ "class" =: "login_info") $ dynText dErrMsg
    name <- divClass "form-group" $ textInput $ def & attributes .~ constDyn ("class" =: "form-control")
    submit <- divClass "form-group" $ buttonAttr "Submit" $ constDyn ("class" =: "btn")
    pwd <- liftIO $ replicateM 6 $ randomRIO ('1', '9')

    let eName = tagPromptlyDyn (value name) submit
        eRightName = ffilter (\s -> T.length s > 4) eName
        eUser = (\n -> User n (T.pack pwd)) <$> eRightName
        eErr = either id (const "" :: Token -> Text) <$> result

    dErrMsg <- holdDyn "" $
      leftmost [ (<> " length less than 4") <$> ffilter (\s -> T.length s <= 4) eName
               , eErr
               , "waiting for response" <$ submit
               , "login success" <$ ffilter isRight result
               ]
    result <- Api.login eUser
  pure result

loginFooter :: MonadWidget t m => m (Event t (), Event t ())
loginFooter =
  divClass "modal-footer form-group" $ do
    cancel <- buttonAttr "Cancel" $ constDyn formControl
    submit <- buttonAttr "OK" $ constDyn formControl
    pure (cancel, submit)

loginModal :: MonadWidget t m => Event t () -> m (Event t (Either Text Token), Dynamic t AttributeMap)
loginModal eToggle =
  divClass "modal-dialog" $ divClass "modal-content" $ do
    eClose <- loginHeader
    --eeUserInfo <- loginBody
    eeUserInfo <- loginBody
    (eCancel, eOk) <- loginFooter
    dModalAttr' <- holdDyn False $
      leftmost [True<$ eToggle, False<$ eClose, False <$ eCancel, False <$ eOk]

    let dModalAttr = visiblility <$> dModalAttr'
    pure (eeUserInfo, dModalAttr)

  where
    visiblility True = displayBlock
    visiblility False = displayNone

loginW :: MonadWidget t m => m (Event t (Maybe Token))
loginW = do
  rec
    modal <- buttonAttr "Login" dAttr
    let dAttr = either (const $ displayBlock <> formControl) (const $ displayNone <> formControl) <$> dUserInfo
        dModalAttr' = (("class" =: "modal") <>) <$> dModalAttr
    (eeUserInfo, dModalAttr) <- elDynAttr "modal" dModalAttr' $
      loginModal modal
    dUserInfo <- holdDyn (Left "") eeUserInfo

  pure $ either (const Nothing) Just <$> eeUserInfo

