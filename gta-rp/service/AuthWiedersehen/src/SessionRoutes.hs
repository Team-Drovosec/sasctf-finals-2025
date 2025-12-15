{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SessionRoutes where

import AppContext
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import MCCrypto
import Models
import Network.HTTP.Types (status200, status400, status401, status404)
import Network.HTTP.Types.Status (status500)
import Network.Wai
import RedisService
import Utils

handleSignout :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSignout ctx request respond =
  handleJsonRequest request respond $ \signoutReq -> do
    let authSvc = authService ctx

    case signoutReq of
      Object obj -> case KM.lookup "accessToken" obj of
        Just (String accessToken) -> do
          invalidateSession authSvc accessToken
          sendEmptySuccess respond
        _ -> sendError respond status400 "Bad Request" "Missing access token" Nothing
      _ -> sendError respond status400 "Bad Request" "Invalid request format" Nothing

handleInvalidate :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleInvalidate ctx request respond =
  handleJsonRequest request respond $ \invalidateReq -> do
    let authSvc = authService ctx

    case invalidateReq of
      Object obj -> case KM.lookup "accessToken" obj of
        Just (String accessToken) -> do
          invalidateSession authSvc accessToken
          sendEmptySuccess respond
        _ -> sendError respond status400 "Bad Request" "Missing access token" Nothing
      _ -> sendError respond status400 "Bad Request" "Invalid request format" Nothing

returnUserProperties :: AppContext -> Request -> IO (Maybe UserData) -> (Response -> IO ResponseReceived) -> IO ResponseReceived
returnUserProperties ctx request user respond = do
  userResult <- user
  case userResult of
    Just userData -> do
      let unsigned =
            case find (\(key, _) -> key == "unsigned") (queryString request) of
              Just (_, Just value) -> value == "true"
              _ -> False
      posixTime <- getPOSIXTime
      let timestamp = floor (posixTime * 1000000000) :: Int
      let textures = Texture timestamp (userId userData) (AppContext.username userData) (Textures Nothing Nothing)
      let texturesJson = Data.Aeson.encode textures
      let texturesBase64 = decodeUtf8 $ B64.encode $ LBS.toStrict texturesJson
      signatureResult <-
        if unsigned
          then return (Right Nothing)
          else do
            signature <- signTextures ctx texturesBase64
            return (fmap Just signature)
      case signatureResult of
        Left err -> sendError respond status400 "Bad Request" ("Failed to sign textures: " <> T.pack (show err)) Nothing
        Right signature -> do
          let properties = [Property "textures" texturesBase64 signature]
          let profileResponse = SessionProfile (userId userData) (AppContext.username userData) properties
          let jsonValue = toJSON profileResponse
          let jsonText = decodeUtf8 $ LBS.toStrict $ Data.Aeson.encode profileResponse
          sendSuccess respond status200 jsonValue
    Nothing -> sendError respond status404 "Not Found" "Profile not found" Nothing

handleSessionProfile :: AppContext -> Request -> (Response -> IO ResponseReceived) -> Text -> IO ResponseReceived
handleSessionProfile ctx request respond uuid = do
  let authSvc = authService ctx

  returnUserProperties ctx request (getUserById authSvc uuid) respond

handlePublicKeys :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePublicKeys ctx request respond = do
  let publicKeyValue = AppContext.publicKey ctx
  let publicKeyObj = PublicKey publicKeyValue
  let response = PublicKeysResponse [publicKeyObj] [publicKeyObj]
  sendSuccess respond status200 (toJSON response)

handleJoin :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleJoin ctx request respond =
  handleJsonRequest request respond $ \joinReq -> do
    let authSvc = authService ctx
    let redisSvc = redisService ctx
    case joinReq of
      Object obj -> case KM.lookup "accessToken" obj of
        Just (String accessToken) -> do
          sessionResult <- validateSession authSvc accessToken
          case sessionResult of
            Just sessionData -> do
              let uuid =
                    KM.lookup "selectedProfile" obj >>= \val -> case val of
                      String uuidStr -> Just uuidStr
                      _ -> Nothing
              let serverId =
                    KM.lookup "serverId" obj >>= \val -> case val of
                      String serverIdStr -> Just serverIdStr
                      _ -> Nothing
              let sessionUuid = sessionUserId sessionData

              case (uuid, serverId) of
                (Just uuidVal, Just serverIdVal)
                  | uuidVal == sessionUuid -> do
                      storeResult <- storeJoinPair redisSvc uuidVal serverIdVal
                      case storeResult of
                        Right True -> sendEmptySuccess respond
                        Right False -> sendEmptySuccess respond
                        Left _ -> sendEmptySuccess respond
                  | otherwise -> sendError respond status401 "Unauthorized" "Selected profile does not match token owner" Nothing
                _ -> sendError respond status400 "Bad Request" "Missing selected profile or serverId" Nothing
            Nothing -> sendError respond status401 "Unauthorized" "Invalid or expired session" Nothing
        _ -> sendError respond status400 "Bad Request" "Missing access token" Nothing
      _ -> sendError respond status400 "Bad Request" "Invalid request format" Nothing

handleHasJoined :: AppContext -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleHasJoined ctx request respond = do
  let authSvc = authService ctx
  let redisSvc = redisService ctx

  let usernameParam = find (\(key, _) -> key == "username") (queryString request)
  let serverIdParam = find (\(key, _) -> key == "serverId") (queryString request)

  case (usernameParam, serverIdParam) of
    (Just (_, Just username), Just (_, Just serverId)) -> do
      let usernameText = decodeUtf8 username
      let serverIdText = decodeUtf8 serverId

      userResult <- getUserByUsername authSvc usernameText
      case userResult of
        Just userData -> do
          let uuid = userId userData
          checkResult <- checkJoinPair redisSvc uuid serverIdText
          case checkResult of
            Right True -> returnUserProperties ctx request (return $ Just userData) respond
            Right False -> sendError respond status400 "Bad Request" "Player has not joined this server" Nothing
            Left _ -> sendError respond status500 "Internal Server Error" "Failed to check join pair" Nothing
        Nothing -> sendError respond status404 "Not Found" "User not found" Nothing
    _ -> sendError respond status400 "Bad Request" "Missing username or serverId parameter" Nothing
