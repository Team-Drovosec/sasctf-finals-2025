{-# LANGUAGE OverloadedStrings #-}

module MCCrypto
  ( signTextures,
    byteStringToBase64,
  )
where

import AppContext (AppContext (..))
import Crypto.Hash (SHA1 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

byteStringToBase64 :: BS.ByteString -> Text
byteStringToBase64 = decodeUtf8 . B64.encode

signTextures :: AppContext -> Text -> IO (Either RSA.Error Text)
signTextures ctx textures = do
  let priv = privateKey ctx
      msg = encodeUtf8 textures
  signatureResult <- PKCS15.signSafer (Just SHA1) priv msg

  case signatureResult of
    Left err -> return (Left err)
    Right sig -> return (Right (byteStringToBase64 sig))
