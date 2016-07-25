{-# LANGUAGE OverloadedStrings #-}
module Api where

import Common.Types
import Reflex
import Reflex.Dom
import Data.Bson (ObjectId)
import Data.Monoid
import Data.String
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Aeson.Types as A
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import Data.Map (Map)
import Codec.Binary.Base64.String
import qualified Storage
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Crypto.JWT
import Crypto.JOSE
import qualified Data.HashMap.Strict as HM
import Control.Lens.Getter ((^.))
import qualified Data.Text as T

host :: IsString a => a
host = "http://localhost:3030/"

fetchByEvent :: (MonadWidget t m, FromJSON b) => Event t XhrRequest -> m (Event t b)
fetchByEvent e =
  fmapMaybe decodeXhrResponse <$> performRequestAsync e

fetchTopic :: (MonadWidget t m) => Event t ObjectId -> m (Event t Topic)
fetchTopic e = do
  let req id' = xhrRequest "GET" (host <> "topic/" <> show id') def

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

postJsonWithHeader :: (ToJSON a) => String -> Map String String -> a -> XhrRequest
postJsonWithHeader url headers a =
  XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                              , _xhrRequestConfig_sendData = Just body}

  where
    headerUrlEnc = "Content-type" =: "application/json" <> headers
    body = LT.unpack . B.toLazyText . encodeToTextBuilder $ toJSON a

postComment'  :: MonadWidget t m => Token ->  Event t Comment -> m (Event t [Comment])
postComment' tok e = do
  let req  = postJsonWithHeader (host <> "comment") ("Authorization" =: ("Bearer " <> T.unpack (token tok)))

  fetchByEvent (req <$> e)

postTopic :: MonadWidget t m => Event t Topic -> m (Event t Topic)
postTopic e = do
  let req = postJson (host <> "topic")

  fetchByEvent (req <$> e)

parseToken :: Token -> Either String UserInfo
parseToken tok =
  case decodeCompact . BL.fromStrict . encodeUtf8 $ token tok of
    Left err -> Left $ show err
    Right jwt ->
      case HM.lookup "user" $ jwtClaimsSet jwt ^. unregisteredClaims of
        Nothing -> Left "claim invalied"
        Just user -> case user of
                        A.String user' -> Right $ UserInfo user'
                        _ -> Left "user info must be text"

login :: MonadWidget t m => Event t User -> m (Event t (Either String Token))
login e = do
  let
      req (User usr pwd) = xhrRequest "GET" (host <> "login") def {
        _xhrRequestConfig_headers =
          "Authorization" =: ("Basic " <> encode (T.unpack usr <> ":" <> T.unpack pwd))
      }
      f :: XhrResponse -> Either String Token
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
