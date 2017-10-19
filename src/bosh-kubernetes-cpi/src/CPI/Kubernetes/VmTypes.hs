{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CPI.Kubernetes.VmTypes(
    VmProperties(..)
  , emptyVmProperties
  , Service(..)
  , Resources(..)
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
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text

import qualified Kubernetes.Model.V1.ObjectMeta  as ObjectMeta
import qualified Kubernetes.Model.V1.Service     as Kubernetes
import qualified Kubernetes.Model.V1.ServicePort as Kubernetes
import qualified Kubernetes.Model.V1.ServiceSpec as Kubernetes

import Control.Applicative

emptyVmProperties :: VmProperties
emptyVmProperties = VmProperties {
    services = mempty
  , resources = mempty
}

data VmProperties = VmProperties {
    services :: [Service]
  , resources :: Resources
} deriving (Show, Eq)

instance FromJSON VmProperties where
  parseJSON (Object o) =  VmProperties
                      <$> o .:? "services" .!= []
                      <*> o .:? "resources" .!= mempty
  parseJSON invalid    = typeMismatch "VmProperties" invalid

data Service = Service {
    serviceName  :: Text
} deriving (Show, Eq)

data Resources = Resources {
    limits :: Maybe (HashMap Text Text)
  , requests :: Maybe (HashMap Text Text)
} deriving (Show, Eq)

instance Monoid Resources where
  mempty = Resources {
      limits = mempty
    , requests = mempty
  }
  mappend l r = l {
      limits = limits l `mappend` limits r
    , requests = requests l `mappend` requests r
  }

instance FromJSON Resources where
  parseJSON = withObject "resources" $ \o -> do
    let toText :: Float -> Text
        toText = Text.pack . show
        fromFloat :: Text -> Object -> Parser (Maybe (HashMap Text Text))
        fromFloat n o = do
          raw :: Maybe (HashMap Text Float) <- o .: n
          case raw of
            Just (r :: HashMap Text Float) -> pure $ Just $ toText <$> r
            Nothing -> pure Nothing
        fromText :: Text -> Object -> Parser (Maybe (HashMap Text Text))
        fromText n o = o .: n
    limits <-  fromText "limits" o
           <|> fromFloat "limits" o
           <|> pure mempty
    requests <-
               fromText "requests" o
           <|> fromFloat "requests" o
           <|> pure mempty
    pure Resources{..}

valueMismatch :: [Text] -> Value -> Parser a
valueMismatch expectedValues value = fail $ Text.unpack $ "expected one of [" <> Text.intercalate ", " expectedValues <> "], encountered '" <> (Text.decodeUtf8 $ toStrict $ encode value) <> "'"

$(deriveJSON defaultOptions{fieldLabelModifier =
              fieldLabelMap [
                          ("serviceName", "name")]}
            ''Service)
