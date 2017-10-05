{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CPI.Kubernetes.VmTypes(
    VmProperties(..)
  , emptyVmProperties
  , services
  , Service(..)
  , serviceName
) where

import           Aeson.Helpers
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Log
import           Control.Monad.Reader
import qualified CPI.Base                        as Base
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8      as ByteString.Lazy
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text

import qualified Kubernetes.Model.V1.ObjectMeta  as ObjectMeta
import qualified Kubernetes.Model.V1.Service     as Kubernetes
import qualified Kubernetes.Model.V1.ServicePort as Kubernetes
import qualified Kubernetes.Model.V1.ServiceSpec as Kubernetes

emptyVmProperties :: VmProperties
emptyVmProperties = VmProperties {
  services = []
}

data VmProperties = VmProperties {
  services :: [Service]
} deriving (Show, Eq)

instance FromJSON VmProperties where
  parseJSON (Object o) =  VmProperties
                      <$> o .:? "services" .!= []
  parseJSON invalid    = typeMismatch "VmProperties" invalid

data Service = Service {
    serviceName  :: Text
} deriving (Show, Eq)

valueMismatch :: [Text] -> Value -> Parser a
valueMismatch expectedValues value = fail $ Text.unpack $ "expected one of [" <> Text.intercalate ", " expectedValues <> "], encountered '" <> (Text.decodeUtf8 $ toStrict $ encode value) <> "'"

$(deriveJSON defaultOptions{fieldLabelModifier =
              fieldLabelMap [
                          ("serviceName", "name")]}
            ''Service)
