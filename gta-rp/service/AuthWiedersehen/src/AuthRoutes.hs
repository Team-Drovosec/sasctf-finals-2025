{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module AuthRoutes where

import AppContext
import Control.Lens
import Data.Aeson (ToJSON (toJSON), Value (Object, String), defaultOptions)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.TH
import Data.Generics.Product (field)
import Data.Text
import qualified Data.Text as T
import GHC.Generics
import Models
import Network.HTTP.Types (status200, status400, status401)
import Network.Wai
import Text.Read (readMaybe)
import Utils

data AuthRequest = AuthRequest
  { agent :: Maybe Agent,
    username :: Text,
    password :: Text,
    clientToken :: Maybe Text
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''AuthRequest

data AuthResponse = AuthResponse
  { accessToken :: Text,
    clientToken :: Text,
    selectedProfile :: Profile,
    availableProfiles :: [Profile],
    user :: User
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''AuthResponse

handleAuthenticate :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAuthenticate ctx request respond =
  handleJsonRequest request respond $ \(authReq :: AuthRequest) -> do
    let username = authReq ^. field @"username"
        password = authReq ^. field @"password"
        authSvc = authService ctx
    let code = readMaybe (T.unpack password)
    case code of
      Nothing -> sendError respond status400 "Bad Request" "Invalid OTP code" Nothing
      Just code -> do
        userResult <- authenticateUser authSvc username code
        case userResult of
          Just userData -> do
            accessToken <- generateToken
            clientToken <- maybe generateToken return (authReq ^. field @"clientToken")
            sessionResult <- createSession authSvc accessToken clientToken (userId userData)
            case sessionResult of
              Right sessionData -> do
                let profile = Profile (userId userData) username
                let authResp = AuthResponse accessToken clientToken profile [profile] (User (userId userData) username [])
                sendSuccess respond status200 (toJSON authResp)
              Left err -> sendError respond status401 "Unauthorized" ("Failed to create session: " <> T.pack err) Nothing
          Nothing -> sendError respond status401 "Unauthorized" "Invalid OTP code" Nothing

handleRefresh :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleRefresh ctx request respond =
  handleJsonRequest request respond $ \(refreshReq :: Value) -> do
    let authSvc = authService ctx

    case refreshReq of
      Object obj -> case KM.lookup "accessToken" obj of
        Just (String accessToken) -> do
          sessionResult <- refreshSession authSvc accessToken
          case sessionResult of
            Just sessionData -> do
              userResult <- getUserById authSvc (sessionUserId sessionData)
              case userResult of
                Just userData -> do
                  let profile = Profile (userId userData) (AppContext.username userData)
                  let authResp = AuthResponse (AppContext.accessToken sessionData) (AppContext.clientToken sessionData) profile [profile] (User (userId userData) (AppContext.username userData) [])
                  sendSuccess respond status200 (toJSON authResp)
                Nothing -> sendError respond status401 "Unauthorized" "User not found" Nothing
            Nothing -> sendError respond status401 "Unauthorized" "Invalid or expired session" Nothing
        _ -> sendError respond status400 "Bad Request" "Missing access token" Nothing
      _ -> sendError respond status400 "Bad Request" "Invalid request format" Nothing

handleValidate :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleValidate ctx request respond =
  handleJsonRequest request respond $ \(validateReq :: Value) -> do
    let authSvc = authService ctx

    case validateReq of
      Object obj -> case KM.lookup "accessToken" obj of
        Just (String accessToken) -> do
          sessionResult <- validateSession authSvc accessToken
          case sessionResult of
            Just _ -> sendEmptySuccess respond
            Nothing -> sendError respond status401 "Unauthorized" "Invalid or expired session" Nothing
        _ -> sendError respond status400 "Bad Request" "Missing access token" Nothing
      _ -> sendError respond status400 "Bad Request" "Invalid request format" Nothing
