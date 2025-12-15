{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppContext
import AuthRoutes
import AuthServiceImpl
import Control.Exception (try)
import Control.Monad.Random (newStdGen)
import Control.Monad.Trans.Random (evalRandT)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import GHC.IORef (newIORef)
import Network.HTTP.Types (status404)
import Network.Wai
import Network.Wai.Handler.Warp
import RedisService
import RootRoute
import SessionRoutes
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import System.Random (initStdGen)
import Utils

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  conn <- connectPostgreSQL "host=postgres port=5432 user=gtarp_admin password=gtarp_secret dbname=gtarp"

  authSvc <- createAuthService conn

  redisResult <- createRedisService
  redisSvc <- case redisResult of
    Left err -> error ("Redis initialization failed: " ++ err)
    Right svc -> return svc

  publicKey <- readPublicKey

  privateKey <- readPrivateKey

  stdGen <- newIORef =<< newStdGen

  let ctx = AppContext conn authSvc redisSvc publicKey privateKey stdGen

  run 25566 (app ctx)

app :: AppContext -> Application
app ctx request respond = do
  let path = rawPathInfo request
      method = requestMethod request

  let pathSplit = tail $ T.splitOn "/" (decodeUtf8 path)
  case (method, pathSplit) of
    ("POST", ["authserver", "authenticate"]) -> handleAuthenticate ctx request respond
    ("POST", ["authserver", "refresh"]) -> handleRefresh ctx request respond
    ("POST", ["authserver", "validate"]) -> handleValidate ctx request respond
    ("POST", ["authserver", "signout"]) -> handleSignout ctx request respond
    ("POST", ["authserver", "invalidate"]) -> handleInvalidate ctx request respond
    ("GET", ["sessionserver", "session", "minecraft", "profile", uuid]) -> handleSessionProfile ctx request respond uuid
    ("GET", ["servicesserver", "publickeys"]) -> handlePublicKeys ctx request respond
    ("POST", ["sessionserver", "session", "minecraft", "join"]) -> handleJoin ctx request respond
    ("GET", ["sessionserver", "session", "minecraft", "hasJoined"]) -> handleHasJoined ctx request respond
    ("POST", ["api", "v1", "register"]) -> handleRegister ctx request respond
    ("GET", [""]) -> handleRoot ctx request respond
    _ -> sendError respond status404 "Not Found" "The requested resource was not found" Nothing
