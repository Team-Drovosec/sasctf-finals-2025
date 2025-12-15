{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.V4 (nextRandom)
import Models
import Network.HTTP.Types (Status, status204, status400)
import Network.Wai
import Network.Wai.Internal (getRequestBodyChunk)

getRequestBody :: Request -> IO LBS.ByteString
getRequestBody request = LBS.fromStrict <$> getRequestBodyChunk request

sendError :: (Response -> IO ResponseReceived) -> Status -> Text -> Text -> Maybe Text -> IO ResponseReceived
sendError respond statusCode errorType message cause = do
  let response =
        responseLBS
          statusCode
          [("Content-Type", "application/json")]
          (encode $ ErrorResponse errorType message cause)
  respond response

sendSuccess :: (Response -> IO ResponseReceived) -> Status -> Value -> IO ResponseReceived
sendSuccess respond statusCode value = do
  let response =
        responseLBS
          statusCode
          [("Content-Type", "application/json")]
          (encode value)
  respond response

sendEmptySuccess :: (Response -> IO ResponseReceived) -> IO ResponseReceived
sendEmptySuccess respond = do
  let response =
        responseLBS
          status204
          [("Content-Type", "application/json")]
          ""
  respond response

handleJsonRequest :: (FromJSON a) => Request -> (Response -> IO ResponseReceived) -> (a -> IO ResponseReceived) -> IO ResponseReceived
handleJsonRequest request respond handler = do
  body <- getRequestBody request
  case eitherDecode body of
    Left err -> sendError respond status400 "Bad Request" ("Invalid JSON: " <> decodeUtf8 (C8.pack err)) Nothing
    Right value -> handler value

generateToken :: IO Text
generateToken = do
  T.replace "-" "" . decodeUtf8 . C8.pack . show <$> nextRandom
