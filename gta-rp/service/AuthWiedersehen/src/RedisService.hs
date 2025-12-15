{-# LANGUAGE OverloadedStrings #-}

module RedisService where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Database.Redis
import System.Environment (lookupEnv)

data RedisService = RedisService
  { redisConnection :: Connection,
    storeJoinPair :: T.Text -> T.Text -> IO (Either String Bool),
    checkJoinPair :: T.Text -> T.Text -> IO (Either String Bool)
  }

createRedisService :: IO (Either String RedisService)
createRedisService = do
  hostEnv <- lookupEnv "REDIS_HOST"
  portEnv <- lookupEnv "REDIS_PORT"

  let host = maybe "localhost" id hostEnv
  let port = maybe 6379 (read) portEnv

  let connectInfo = defaultConnectInfo {connectHost = host, connectPort = PortNumber $ fromIntegral port}

  conn <- connect connectInfo
  return $
    Right
      RedisService
        { redisConnection = conn,
          storeJoinPair = storeJoinPairImpl conn,
          checkJoinPair = checkJoinPairImpl conn
        }

storeJoinPairImpl :: Connection -> T.Text -> T.Text -> IO (Either String Bool)
storeJoinPairImpl conn uuid serverId = do
  let key = "join:" <> uuid <> ":" <> serverId
  result <- runRedis conn $ setex (BS.pack $ T.unpack key) 300 "1"
  case result of
    Right Ok -> return $ Right True
    Right _ -> return $ Right False
    Left err -> return $ Left $ "Redis error: " ++ show err

checkJoinPairImpl :: Connection -> T.Text -> T.Text -> IO (Either String Bool)
checkJoinPairImpl conn uuid serverId = do
  let key = "join:" <> uuid <> ":" <> serverId
  result <- runRedis conn $ get (BS.pack $ T.unpack key)
  case result of
    Right (Just _) -> return $ Right True
    Right Nothing -> return $ Right False
    Left err -> return $ Left $ "Redis error: " ++ show err
