{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CPI.Kubernetes.VmPropertiesLens(
    services
  , serviceName
  , resources
  , limits
  , requests
) where

import qualified CPI.Kubernetes.VmTypes as VmProperties

import           Aeson.Helpers
import           Control.Lens
import Control.Lens.Helper.LensNames
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


makeLensesWith sameNameRules ''VmProperties.VmProperties
makeLensesWith sameNameRules ''VmProperties.Service
makeLensesWith sameNameRules ''VmProperties.Resources

