{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module CPI.Kubernetes.Config(
    Config(..)
  , HasConfig(..)
  , parseConfig
  , parseClusterAccess
  , ClusterAccess(..)
  , Credentials(..)
) where

import           Prelude                  hiding (readFile)

import qualified CPI.Base                 as Base

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.FileSystem
import           CPI.Base.System
import           Data.Aeson.Types
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Data.Yaml
import           GHC.Generics

import           Network.TLS              (Credential,
                                           credentialLoadX509FromMemory)
import qualified Servant.Common.BaseUrl   as Url

class HasConfig a where
  asConfig :: a -> Config
  fromConfig :: Config -> a

instance HasConfig Config where
  asConfig = id
  fromConfig = id

parseCredentials :: (MonadThrow m) => RawCredentials -> m Credentials
parseCredentials (RawClientCertificate rawCertificate rawPrivateKey) = do
  creds <- readCredential
            (encodeUtf8 rawCertificate)
            (encodeUtf8 rawPrivateKey)
  pure $ ClientCertificate creds
parseCredentials (RawToken rawToken) =
  pure $ Token rawToken

parseConfig :: (MonadThrow m) => ByteString -> m Config
parseConfig input = do
  RawConfig clusterAccess' agent <- readConfig input
  clusterAccess <- parseClusterAccess clusterAccess'
  pure Config {
    clusterAccess = clusterAccess
  , agent = agent
}

parseClusterAccess :: (MonadThrow m) => RawClusterAccess -> m ClusterAccess
parseClusterAccess RawServiceAccount = do
  server <- Url.parseBaseUrl "https://kubernetes"
  pure ClusterAccess {
      server = pure server
    , namespace = decodeUtf8 <$> readFile "/var/run/secrets/kubernetes.io/serviceaccount/namespace"
    , credentials = Token . decodeUtf8 <$> readFile "/var/run/secrets/kubernetes.io/serviceaccount/token"
  }
parseClusterAccess RawClusterAccess {
    _server = server
  , _namespace = namespace
  , _credentials = credentials
} = do
  server <- Url.parseBaseUrl $ Text.unpack server
  credentials <- parseCredentials credentials
  pure ClusterAccess {
      server = pure server
    , namespace = pure namespace
    , credentials = pure credentials
  }

data Config = Config {
    clusterAccess :: ClusterAccess
  , agent         :: Object
}

data ClusterAccess = ClusterAccess {
    server      :: forall m. (MonadFileSystem m) => m Url.BaseUrl
  , credentials :: forall m. (MonadFileSystem m) => m Credentials
  , namespace   :: forall m. (MonadFileSystem m) => m Text
}

data Credentials = ClientCertificate Credential
                 | Token Text


readConfig :: (MonadThrow m, FromJSON c) => BS.ByteString -> m c
readConfig input = either throwM return $ decodeEither' input

readCredential :: (MonadThrow m) => BS.ByteString -> BS.ByteString -> m Credential
readCredential cert key = either (throwM . Base.ConfigParseException) return $ credentialLoadX509FromMemory cert key

data RawConfig = RawConfig {
    _clusterAccess :: RawClusterAccess
  , _agent         :: Object
} deriving (Show, Generic)

instance FromJSON RawConfig where
  parseJSON (Object v) = RawConfig    <$>
                         v .: "access" <*>
                         v .: "agent"

  parseJSON invalid    = typeMismatch "Config" invalid

data RawClusterAccess = RawServiceAccount
                      | RawClusterAccess {
                          _server      :: Text
                        , _namespace   :: Text
                        , _credentials :: RawCredentials
                      } deriving (Show, Generic)

instance FromJSON RawClusterAccess where
  parseJSON (Object v) = RawClusterAccess    <$>
                           v .: "server" <*>
                           v .: "namespace" <*>
                           v .: "credentials"
  parseJSON (String "ServiceAccount") = pure RawServiceAccount

  parseJSON invalid    = typeMismatch "ClusterAccess" invalid


data RawCredentials = RawToken Text
                    | RawClientCertificate {
                      _certificate :: Text,
                      _privateKey  :: Text
                    } deriving (Show, Generic)

instance FromJSON RawCredentials where
  parseJSON (Object v) =  RawToken
                         <$> v .: "token"
                      <|> RawClientCertificate
                         <$> v .: "certificate"
                         <*> v .: "private_key"

  parseJSON invalid    = typeMismatch "Credentials" invalid
