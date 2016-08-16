{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Api where

import Common.Types
import Reflex
import Reflex.Dom
import Data.Bson (ObjectId)
import Data.Monoid
import Data.String
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson.Encode as AE
import qualified Data.Aeson.Types as A
--import qualified Data.Text.Lazy as LT
--import qualified Data.Text.Lazy.Builder as B
import Data.Map (Map)
import Codec.Binary.Base64.String
import qualified Storage
import qualified Data.ByteString.Lazy as BL
import Crypto.JWT
import Crypto.JOSE
import qualified Data.HashMap.Strict as HM
import Control.Lens.Getter ((^.))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHCJS.DOM.XMLHttpRequest (sendString)

instance IsXhrPayload Text where
  sendXhrPayload = sendString
host :: IsString a => a
#ifdef PRODUCT
host = "http://191.101.14.233:3030/"
#else
host = "http://localhost:3030/"
#endif

fetchByEvent :: (MonadWidget t m, FromJSON b, IsXhrPayload a) => Event t (XhrRequest a) -> m (Event t b)
fetchByEvent e =
  fmapMaybe decodeXhrResponse <$> performRequestAsync e

fetchTopic :: (MonadWidget t m) => Event t ObjectId -> m (Event t Topic)
fetchTopic e = do
  let req id' = xhrRequest "GET" (host <> "topic/" <> T.pack (show id')) def

  fetchByEvent $ req <$> e
getNewestTopic :: MonadWidget t m => m (Event t Topic)
getNewestTopic = do
  let req = xhrRequest "GET" (host <> "topic/newest") def
  e <- getPostBuild
  fetchByEvent (req <$ e)

getTopicList :: MonadWidget t m => m (Event t [Topic])
getTopicList = do
  let req = xhrRequest "GET" (host <> "topics") def
  event <- getPostBuild
  fetchByEvent (req <$ event)

fetchTopicList :: MonadWidget t m => Event t a -> m (Event t [Topic])
fetchTopicList e = do
  let req = xhrRequest "GET" (host <> "topics") def
  fetchByEvent (req <$ e)

postComment :: MonadWidget t m  => Comment -> m (Event t [Comment])
postComment c = do
  let req = postJson (host <> "comment") c

  e <- getPostBuild
  fetchByEvent (req <$ e)

postJsonWithHeader :: (ToJSON a) => Text -> Map Text Text-> a -> XhrRequest Text
postJsonWithHeader url headers a =
  XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                              , _xhrRequestConfig_sendData = body}

  where
    headerUrlEnc = "Content-type" =: "application/json" <> headers
    body = TE.decodeUtf8 . BL.toStrict . AE.encode $ a

postComment'  :: MonadWidget t m => Token ->  Event t Comment -> m (Event t [Comment])
postComment' tok e = do
  let req  = postJsonWithHeader (host <> "comment") ("Authorization" =: ("Bearer " <> token tok))

  fetchByEvent (req <$> e)

postTopic :: MonadWidget t m => Event t Topic -> m (Event t Topic)
postTopic e = do
  let req = postJson (host <> "topic")

  fetchByEvent (req <$> e)

parseToken :: Token -> Either Text UserInfo
parseToken tok =
  case decodeCompact . BL.fromStrict . TE.encodeUtf8 $ token tok of
    Left err -> Left . T.pack . show $ err
    Right jwt ->
      case HM.lookup "user" $ jwtClaimsSet jwt ^. unregisteredClaims of
        Nothing -> Left "claim invalied"
        Just user -> case user of
                        A.String user' -> Right $ UserInfo user'
                        _ -> Left "user info must be text"

login :: MonadWidget t m => Event t User -> m (Event t (Either Text Token))
login e = do
  let
      req (User usr pwd) = xhrRequest "GET" (host <> "login") def {
        _xhrRequestConfig_headers =
          "Authorization" =: ("Basic " <> (T.pack . encode . T.unpack $ usr <> ":" <> pwd))
      }
      f :: XhrResponse -> Either Text Token
      f res = case _xhrResponse_status res of
                200 -> case decodeXhrResponse res of
                        Nothing -> Left "sth wrong happened"
                        Just tok -> Right tok
                401 -> Left "not valid user"
                _ -> Left "none"

  eeUserInfo <- fmap f <$> performRequestAsync (req <$> e)
  let eSucLogin = fmapMaybe (either (const Nothing) Just) eeUserInfo
  performEvent_ $ Storage.store "token" <$> eSucLogin
  pure eeUserInfo

confirmUser :: MonadWidget t m => m (Event t (Maybe Token))
confirmUser = do
  e <- getPostBuild
  performEvent $ Storage.read "token" <$ e
