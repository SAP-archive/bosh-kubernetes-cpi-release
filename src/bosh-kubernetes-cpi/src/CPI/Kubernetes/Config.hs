{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module CPI.Kubernetes.Config(
    Config(..)
  , parseConfig
) where

import qualified CPI.Base               as Base

import           Control.Monad.Catch
import           Data.Aeson.Types
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Yaml
import           GHC.Generics
import           Network.TLS
import qualified Servant.Common.BaseUrl as Url

parseConfig :: ByteString -> IO Config
parseConfig input = do
  rawConfig <- readConfig input
  apiEndpoint <- Url.parseBaseUrl $ Text.unpack $ _apiEndpoint rawConfig
  creds <- readCredential
              (encodeUtf8 (_certificate . _credentials $ rawConfig))
              (encodeUtf8 (_privateKey . _credentials $ rawConfig))
  return Config {
    apiEndpoint = apiEndpoint,
    namespace = _namespace rawConfig,
    credentials = creds,
    agent = _agent rawConfig
  }

data Config = Config {
  apiEndpoint :: Url.BaseUrl,
  namespace   :: Text,
  credentials :: Credential,
  agent       :: Object
}


readConfig :: (MonadThrow m, FromJSON c) => BS.ByteString -> m c
readConfig input = either throwM return $ decodeEither' input

readCredential :: (MonadThrow m) => BS.ByteString -> BS.ByteString -> m Credential
readCredential cert key = either (throwM . Base.ConfigParseException) return $ credentialLoadX509FromMemory cert key

data RawConfig = RawConfig {
  _apiEndpoint :: Text,
  _namespace   :: Text,
  _credentials :: RawCredentials,
  _agent       :: Object
} deriving (Show, Generic)

instance FromJSON RawConfig where
  parseJSON (Object v) = RawConfig    <$>
                         v .: "apiEndpoint" <*>
                         v .: "namespace" <*>
                         v .: "credentials" <*>
                         v .: "agent"

  parseJSON invalid    = typeMismatch "Config" invalid


data RawCredentials = RawCredentials {
  _certificate :: Text,
  _privateKey  :: Text
} deriving (Show, Generic)

instance FromJSON RawCredentials where
  parseJSON (Object v) = RawCredentials    <$>
                         v .: "certificate" <*>
                         v .: "privateKey"

  parseJSON invalid    = typeMismatch "Credentials" invalid
