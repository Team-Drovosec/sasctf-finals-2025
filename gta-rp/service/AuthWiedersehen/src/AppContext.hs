{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AppContext where

import Control.Monad (unless)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Random.Strict (Rand, RandT, getRandom, runRandT)
import Control.Monad.Reader
import Crypto.PubKey.RSA (PrivateKey)
import Crypto.PubKey.RSA.Types (PrivateKey (PrivateKey))
import Crypto.Store.PKCS8
import Data.Aeson (ToJSON, defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Base32 as B32
import Data.ByteString.Builder (toLazyByteString, word64BE)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.X509 (PrivKey (PrivKeyRSA))
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import GHC.IORef (readIORef, writeIORef)
import GHC.Word (Word64)
import RedisService
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (callProcess, readProcess)
import System.Random (StdGen, randomIO)

data UserData = UserData
  { userId :: Text,
    username :: Text,
    otpSecret :: Text
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''UserData

data SessionData = SessionData
  { sessionId :: Text,
    accessToken :: Text,
    clientToken :: Text,
    sessionUserId :: Text,
    expiresAt :: UTCTime
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''SessionData

data AuthService = AuthService
  { authenticateUser :: Text -> Int -> IO (Maybe UserData),
    createUser :: Text -> Text -> Text -> IO (Either String UserData),
    getUserByUsername :: Text -> IO (Maybe UserData),
    getUserById :: Text -> IO (Maybe UserData),
    createSession :: Text -> Text -> Text -> IO (Either String SessionData),
    validateSession :: Text -> IO (Maybe SessionData),
    invalidateSession :: Text -> IO (),
    refreshSession :: Text -> IO (Maybe SessionData)
  }

data AppContext = AppContext
  { dbConnection :: Connection,
    authService :: AuthService,
    redisService :: RedisService,
    publicKey :: Text,
    privateKey :: PrivateKey,
    randGen :: IORef StdGen
  }

type AppM = ReaderT AppContext (RandT StdGen IO)

runAppM :: AppContext -> AppM a -> IO a
runAppM ctx action = do
  gen <- readIORef (randGen ctx)
  (result, gen') <- runRandT (runReaderT action ctx) gen
  writeIORef (randGen ctx) gen'
  return result

getDB :: AppM Connection
getDB = asks dbConnection

generateRandomOTPSecret :: AppM Text
generateRandomOTPSecret = do
  a <- getRandom
  b <- getRandom
  let ab = toLazyByteString (word64BE a <> word64BE b)
      secret = B32.encodeBase32Unpadded (LBS.toStrict ab)
  return secret

keysDirectory :: FilePath
keysDirectory = "keys"

privateKeyPath :: FilePath
privateKeyPath = "keys/private_key.pem"

ensurePrivateKeyExists :: IO ()
ensurePrivateKeyExists = do
  createDirectoryIfMissing True keysDirectory
  exists <- doesFileExist privateKeyPath
  unless exists $ do
    callProcess
      "openssl"
      [ "genpkey",
        "-algorithm",
        "RSA",
        "-out",
        privateKeyPath,
        "-pkeyopt",
        "rsa_keygen_bits:2048"
      ]

readPublicKey :: IO Text
readPublicKey = do
  ensurePrivateKeyExists
  let cmd = "openssl pkey -in " ++ privateKeyPath ++ " -pubout -outform DER | base64"
  result <- readProcess "sh" ["-c", cmd] ""
  let cleaned = T.filter (`notElem` ['\n', '\r']) (T.pack result)
  return $ T.strip cleaned

readPrivateKey :: IO PrivateKey
readPrivateKey = do
  ensurePrivateKeyExists
  [key] <- readKeyFile privateKeyPath
  case key of
    Unprotected keyPair ->
      case keyPairToPrivKey keyPair of
        PrivKeyRSA rsaKey -> return rsaKey
        _ -> error "Unsupported private key type"
    _ -> error "Failed to read private key"
