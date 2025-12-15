{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models where

import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

data Agent = Agent
  { name :: Text,
    version :: Int
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''Agent

data Profile = Profile
  { id :: Text,
    name :: Text
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''Profile

data Property = Property
  { name :: Text,
    value :: Text,
    signature :: Maybe Text
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''Property

data SessionProfile = SessionProfile
  { id :: Text,
    name :: Text,
    properties :: [Property]
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''SessionProfile

newtype PublicKey = PublicKey {publicKey :: Text}
  deriving (Show, Generic)

deriveJSON defaultOptions ''PublicKey

data PublicKeysResponse = PublicKeysResponse
  { playerCertificateKeys :: [PublicKey],
    profilePropertyKeys :: [PublicKey]
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''PublicKeysResponse

newtype TexutreSkin = TexutreSkin {url :: Text}
  deriving (Show, Generic)

deriveJSON defaultOptions ''TexutreSkin

data Textures = Textures
  { skin :: Maybe TexutreSkin,
    cape :: Maybe TexutreSkin
  }
  deriving (Show, Generic)

deriveJSON
  defaultOptions
    { fieldLabelModifier = \s -> case s of
        "skin" -> "SKIN"
        "cape" -> "CAPE"
        _ -> s,
      omitNothingFields = True
    }
  ''Textures

data Texture = Texture
  { timestamp :: Int,
    profileId :: Text,
    profileName :: Text,
    textures :: Textures
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''Texture

data User = User
  { id :: Text,
    username :: Text,
    properties :: [Property]
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''User

data ErrorResponse = ErrorResponse
  { error :: Text,
    errorMessage :: Text,
    cause :: Maybe Text
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''ErrorResponse
