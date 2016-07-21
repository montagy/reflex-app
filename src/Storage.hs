{-# LANGUAGE OverloadedStrings #-}
module Storage where

import Data.Aeson (encode, ToJSON, FromJSON, eitherDecode)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Control.Monad.IO.Class (liftIO, MonadIO)
import JavaScript.Web.Storage (setItem, localStorage, getItem)
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Text (Text)

store :: (ToJSON a, MonadIO m) => Text -> a -> m ()
store k v =
  liftIO $ setItem (textToJSString k) (textToJSString . decodeUtf8 . toStrict . encode $ v) localStorage

read :: (FromJSON a, MonadIO m) => Text -> m (Maybe a)
read k = liftIO $ do
  mJss <- getItem (textToJSString k) localStorage
  case mJss of
    Nothing -> return Nothing
    Just jss -> case eitherDecode (fromStrict . encodeUtf8 . textFromJSString $ jss) of
                  Left _ -> return Nothing
                  Right result -> return $ Just result
