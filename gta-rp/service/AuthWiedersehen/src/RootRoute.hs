{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RootRoute where

import AppContext (AppContext, AppM, AuthService (createUser), UserData (UserData), authService, generateRandomOTPSecret, otpSecret, publicKey, runAppM)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, defaultOptions, encode, object, (.=))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (toJSON)
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Product (field)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TUTF8
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Models
import Network.HTTP.Types (status200, status400)
import Network.Wai
import Text.Regex.TDFA
import Utils (handleJsonRequest, sendError, sendSuccess)

handleRoot :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleRoot ctx request respond = do
  htmlContent <- TUTF8.readFile "index.html"
  isBrowser <- isBrowser request
  if isBrowser
    then
      respond $
        responseBuilder
          status200
          [ ("Content-Type", C8.pack "text/html; charset=utf-8")
          ]
          (byteString $ encodeUtf8 htmlContent)
    else do
      let publicKeyValue = AppContext.publicKey ctx
      let formattedPublicKey = T.intercalate "\n" (T.chunksOf 64 publicKeyValue)
      let pemPublicKey = "-----BEGIN PUBLIC KEY-----\n" <> formattedPublicKey <> "\n-----END PUBLIC KEY-----\n"
      let metaInfo =
            object
              [ "meta"
                  .= object
                    [ "serverName" .= ("gtarp auth server" :: Text),
                      "implementationName" .= ("AuthWiedersehen" :: Text),
                      "implementationVersion" .= ("1.0.0" :: Text),
                      "feature.no_mojang_namespace" .= True,
                      "feature.enable_profile_key" .= True
                    ],
                "signaturePublickey" .= (pemPublicKey :: Text),
                "signaturePublickeys" .= [pemPublicKey]
              ]
      let metaInfoJson = encode metaInfo
      respond $
        responseBuilder
          status200
          [ ("Content-Type", C8.pack "application/json; charset=utf-8")
          ]
          (byteString $ LBS.toStrict metaInfoJson)

isBrowser :: Request -> IO Bool
isBrowser request = do
  let headers = requestHeaders request
  return $ any (\(key, value) -> key == "User-Agent" && T.isInfixOf "Mozilla/5.0" (T.pack (C8.unpack value))) headers

data RegisterRequest = RegisterRequest
  { username :: Text,
    userClass :: Text
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''RegisterRequest

parseUserClass :: Text -> Maybe Text
parseUserClass "miner" = Just "miner"
parseUserClass "businessman" = Just "businessman"
parseUserClass _ = Nothing

validateUsername :: Text -> Bool
validateUsername username =
  let usernameStr = T.unpack username
      regex = "^[A-Za-z0-9_]{3,16}$" :: String
   in usernameStr =~ regex

handleRegister :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleRegister ctx request respond =
  handleJsonRequest request respond $
    \(registerReq :: RegisterRequest) -> runAppM ctx $ do
      let authSvc = authService ctx
      let username = registerReq ^. field @"username"
      let userClass = parseUserClass (registerReq ^. field @"userClass")

      if not (validateUsername username)
        then liftIO $ sendError respond status400 "Invalid Username" "Username must be 3-16 characters long and contain only letters, numbers, and underscores" Nothing
        else do
          newOtpSecret <- generateRandomOTPSecret
          case userClass of
            Just userClass -> do
              userData <- liftIO $ createUser authSvc username newOtpSecret userClass
              case userData of
                Left err -> liftIO $ sendError respond status400 "Registration Failed" (T.pack err) Nothing
                Right user -> liftIO $ sendSuccess respond status200 (object ["otpSecret" .= otpSecret user])
            Nothing -> liftIO $ sendError respond status400 "Invalid User Class" "Invalid User Class" Nothing
