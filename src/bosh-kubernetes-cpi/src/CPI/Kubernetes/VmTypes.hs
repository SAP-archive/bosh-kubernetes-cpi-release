{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CPI.Kubernetes.VmTypes(
    VmProperties(..)
  , services
  , Service(..)
  , serviceName
  , serviceType
  , servicePorts
  , ServiceType(..)
  , ServicePort(..)
  , portName
  , port
  , nodePort
  , parseVmProperties
  , createServices
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

import qualified CPI.Kubernetes.Model            as Model

import qualified Kubernetes.Model.V1.ObjectMeta  as ObjectMeta
import qualified Kubernetes.Model.V1.Service     as Kubernetes
import qualified Kubernetes.Model.V1.ServicePort as Kubernetes
import qualified Kubernetes.Model.V1.ServiceSpec as Kubernetes

parseVmProperties :: (MonadThrow m) => Base.VmProperties -> m VmProperties
parseVmProperties (Base.VmProperties base) = case fromJSON base of
  Success value -> pure value
  Data.Aeson.Error msg -> throwM $ Base.CloudError $ "Could not parse 'VmProperties': " <> Text.pack msg

data VmProperties = VmProperties {
  _services :: [Service]
} deriving (Show, Eq)

instance FromJSON VmProperties where
  parseJSON (Object o) =  VmProperties
                      <$> o .:? "services" .!= []
  parseJSON invalid    = typeMismatch "VmProperties" invalid


data Service = Service {
    _serviceName  :: Text
  , _serviceType  :: ServiceType
  , _servicePorts :: [ServicePort]
} deriving (Show, Eq)

data ServiceType = NodePort
  deriving (Show, Eq)


instance FromJSON ServiceType where
  parseJSON (String "NodePort") = pure NodePort
  parseJSON invalid             = valueMismatch ["NodePort"] invalid

instance ToJSON ServiceType where
  toJSON NodePort = String "NodePort"

data ServicePort = ServicePort {
    _portName :: Text
  , _port     :: Integer
  , _nodePort :: Integer
} deriving (Show, Eq)

valueMismatch :: [Text] -> Value -> Parser a
valueMismatch expectedValues value = fail $ Text.unpack $ "expected one of [" <> Text.intercalate ", " expectedValues <> "], encountered '" <> (Text.decodeUtf8 $ toStrict $ encode value) <> "'"

makeLenses ''VmProperties
makeLenses ''Service
makeLenses ''ServiceType
makeLenses ''ServicePort

$(deriveJSON defaultOptions{fieldLabelModifier =
               fieldLabelMap [
                          ("serviceName", "name")
                        , ("serviceType", "type")
                        , ("servicePorts", "ports")]}
            ''Service)
$(deriveJSON defaultOptions{fieldLabelModifier = fieldLabelMap [("portName", "name")]} ''ServicePort)


createServices :: VmProperties -> [Kubernetes.Service]
createServices vmType = createService <$> vmType ^. services

createService :: Service -> Kubernetes.Service
createService service =
  let name = service ^. serviceName
      sType = service ^. serviceType
      ports = service ^. servicePorts
      serviceSpec = Kubernetes.mkServiceSpec (createServicePort <$> ports)
                    & Kubernetes.type_ .~ Just (Text.pack $ show sType)
  in
    Kubernetes.mkService
      & Kubernetes.metadata .~ Just (ObjectMeta.mkObjectMeta
        & ObjectMeta.name .~ Just name)
      & Kubernetes.spec .~ Just serviceSpec

createServicePort :: ServicePort -> Kubernetes.ServicePort
createServicePort portSpec =
  Kubernetes.mkServicePort (portSpec ^. port)
         & Kubernetes.name     .~ Just (portSpec ^. portName)
         & Kubernetes.nodePort .~ Just (portSpec ^. nodePort)
