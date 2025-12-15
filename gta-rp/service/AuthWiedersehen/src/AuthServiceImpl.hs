{-# LANGUAGE OverloadedStrings #-}

module AuthServiceImpl where

import AppContext
import Control.Monad
import Data.ByteString.Base32 (decodeBase32)
import Data.OTP (HashAlgorithm (SHA1), Secret, totpCheck)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

instance FromRow UserData where
  fromRow = UserData <$> field <*> field <*> field

instance FromRow SessionData where
  fromRow = SessionData <$> field <*> field <*> field <*> field <*> field

instance ToRow UserData where
  toRow (UserData uid username otpSecret) =
    toRow (uid, username, otpSecret)

instance ToRow SessionData where
  toRow (SessionData sid accessToken clientToken uid expiresAt) =
    toRow (sid, accessToken, clientToken, uid, expiresAt)

createAuthService :: Connection -> IO AuthService
createAuthService conn = do
  return
    AuthService
      { authenticateUser = authenticateUserImpl conn,
        createUser = createUserImpl conn,
        getUserByUsername = getUserByUsernameImpl conn,
        getUserById = getUserByIdImpl conn,
        createSession = createSessionImpl conn,
        validateSession = validateSessionImpl conn,
        invalidateSession = invalidateSessionImpl conn,
        refreshSession = refreshSessionImpl conn
      }

authenticateUserImpl :: Connection -> Text -> Int -> IO (Maybe UserData)
authenticateUserImpl conn username otpCode = do
  users <- query conn "SELECT id, username, otp_secret FROM users WHERE username = ?" (Only username)
  case users of
    [user] -> do
      case decodeBase32 (encodeUtf8 (otpSecret user)) of
        Left _ -> return Nothing
        Right secret -> do
          isValid <- verifyOTPImpl conn secret otpCode
          if isValid then return (Just user) else return Nothing
    _ -> return Nothing

createUserImpl :: Connection -> Text -> Text -> Text -> IO (Either String UserData)
createUserImpl conn username otpSecret userRole = do
  existing <- query conn "SELECT id FROM users WHERE username = ?" (Only username) :: IO [Only Text]
  case existing of
    [] -> do
      userId <- T.replace "-" "" . T.pack . show <$> nextRandom
      withTransaction conn $ do
        let userData = UserData userId username otpSecret
        _ <- execute conn "INSERT INTO users (id, username, otp_secret) VALUES (?, ?, ?)" userData
        when (userRole == "businessman") $
          void $
            execute
              conn
              "INSERT INTO mine_owners (owner_name) VALUES (?) ON CONFLICT (owner_name) DO NOTHING"
              (Only username)
        return (Right userData)
    _ -> return (Left "User already exists")

getUserByUsernameImpl :: Connection -> Text -> IO (Maybe UserData)
getUserByUsernameImpl conn username = do
  users <- query conn "SELECT id, username, otp_secret FROM users WHERE username = ?" (Only username)
  case users of
    [user] -> return (Just user)
    _ -> return Nothing

getUserByIdImpl :: Connection -> Text -> IO (Maybe UserData)
getUserByIdImpl conn userId = do
  users <- query conn "SELECT id, username, otp_secret FROM users WHERE id = ?" (Only userId)
  case users of
    [user] -> return (Just user)
    _ -> return Nothing

verifyOTPImpl :: Connection -> Secret -> Int -> IO Bool
verifyOTPImpl conn secret otpCode = do
  now <- getCurrentTime
  return $ totpCheck SHA1 secret (5, 5) now 30 8 (fromIntegral otpCode)

createSessionImpl :: Connection -> Text -> Text -> Text -> IO (Either String SessionData)
createSessionImpl conn accessToken clientToken userId = do
  sessionId <- T.replace "-" "" . T.pack . show <$> nextRandom
  now <- getCurrentTime
  let expiresAt = addUTCTime (24 * 60 * 60) now
  let sessionData = SessionData sessionId accessToken clientToken userId expiresAt
  _ <- execute conn "INSERT INTO sessions (id, access_token, client_token, user_id, expires_at) VALUES (?, ?, ?, ?, ?)" sessionData
  return (Right sessionData)

validateSessionImpl :: Connection -> Text -> IO (Maybe SessionData)
validateSessionImpl conn accessToken = do
  sessions <- query conn "SELECT id, access_token, client_token, user_id, expires_at FROM sessions WHERE access_token = ?" (Only accessToken)
  case sessions of
    [session] -> do
      now <- getCurrentTime
      if expiresAt session > now
        then return (Just session)
        else do
          _ <- execute conn "DELETE FROM sessions WHERE access_token = ?" (Only accessToken)
          return Nothing
    _ -> return Nothing

invalidateSessionImpl :: Connection -> Text -> IO ()
invalidateSessionImpl conn accessToken = do
  _ <- execute conn "DELETE FROM sessions WHERE access_token = ?" (Only accessToken)
  return ()

refreshSessionImpl :: Connection -> Text -> IO (Maybe SessionData)
refreshSessionImpl conn accessToken = do
  sessions <- query conn "SELECT id, access_token, client_token, user_id, expires_at FROM sessions WHERE access_token = ?" (Only accessToken)
  case sessions of
    [oldSession] -> do
      newSessionId <- T.pack . show <$> nextRandom
      newAccessToken <- T.pack . show <$> nextRandom
      now <- getCurrentTime
      let expiresAt = addUTCTime (24 * 60 * 60) now
      let newSession = SessionData newSessionId newAccessToken (clientToken oldSession) (sessionUserId oldSession) expiresAt

      _ <- execute conn "DELETE FROM sessions WHERE access_token = ?" (Only accessToken)
      _ <- execute conn "INSERT INTO sessions (id, access_token, client_token, user_id, expires_at) VALUES (?, ?, ?, ?, ?)" newSession
      return (Just newSession)
    _ -> return Nothing
