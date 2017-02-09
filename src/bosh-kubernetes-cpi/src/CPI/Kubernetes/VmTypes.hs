{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module CPI.Kubernetes.VmTypes(
    createServiceSpec
) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Log
import           Control.Monad.Reader
import           CPI.Base                        (VmProperties)
import           CPI.Base.Networks
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe
import           Data.Text                       (Text)

import qualified CPI.Kubernetes.Model            as Model

import qualified Kubernetes.Model.V1.Service     as Service
import qualified Kubernetes.Model.V1.ServicePort as ServicePort
import qualified Kubernetes.Model.V1.ServiceSpec as ServiceSpec


createServiceSpec :: Value -> ServiceSpec.ServiceSpec
createServiceSpec vmType = let
  servicePorts = catMaybes $ createServicePort <$> vmType ^.. service.ports.values
  in ServiceSpec.mkServiceSpec servicePorts
                & ServiceSpec.type_ .~ vmType ^? service.type_

createServicePort :: Value -> Maybe ServicePort.ServicePort
createServicePort portSpec = do
  targetPort <- portSpec ^? port
  return $ ServicePort.mkServicePort targetPort
         & ServicePort.name     .~ portSpec ^? name
         & ServicePort.nodePort .~ portSpec ^? nodePort

service :: Traversal' Value Value
service = key "service"

type_ :: Traversal' Value Text
type_ = key "type" . _String

ports :: Traversal' Value Value
ports = key "ports"

name :: Traversal' Value Text
name = key "name" . _String

port :: Traversal' Value Integer
port = key "port" . _Integer

nodePort :: Traversal' Value Integer
nodePort = key "nodePort" . _Integer
