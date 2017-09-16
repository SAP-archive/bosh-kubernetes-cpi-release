{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Kubernetes.Resource.Service(
    Service
  , ServiceList
  , services
  , MonadService(..)
  , newService
  , newServiceSpec
  , newServicePort
  , podSelector
) where

import qualified CPI.Base                            as Base
import           CPI.Base.Errors                     (CloudError (..))
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Servant


import           CPI.Kubernetes.Resource.Metadata
import           Resource

import           Kubernetes.Model.Unversioned.Status (Status)
import qualified Kubernetes.Model.V1.Any             as Any
import           Kubernetes.Model.V1.DeleteOptions   (mkDeleteOptions)
import           Kubernetes.Model.V1.ObjectMeta      (ObjectMeta, mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta      as ObjectMeta
import           Kubernetes.Model.V1.Service         (Service, mkService)
import qualified Kubernetes.Model.V1.Service         as Service
import           Kubernetes.Model.V1.ServiceList     (ServiceList)
import qualified Kubernetes.Model.V1.ServiceList     as ServiceList
import           Kubernetes.Model.V1.ServicePort     (ServicePort,
                                                      mkServicePort)
import qualified Kubernetes.Model.V1.ServicePort     as ServicePort
import           Kubernetes.Model.V1.ServiceSpec     (ServiceSpec,
                                                      mkServiceSpec)
import qualified Kubernetes.Model.V1.ServiceSpec     as ServiceSpec

import           Kubernetes.Api.ApivApi              (createNamespacedService,
                                                      deleteNamespacedService,
                                                      listNamespacedService,
                                                      readNamespacedService,
                                                      replaceNamespacedService)

import           Control.Monad.Reader

import           Control.Exception.Safe
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad.Console
import           Control.Monad.FileSystem
import           Control.Monad.Log
import           Servant.Client

import           Control.Monad.Wait
import           Data.Aeson
import           Data.ByteString.Lazy                (toStrict)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Hourglass.Types
import           Data.List
import           Data.Semigroup
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.Text.Encoding                  (decodeUtf8)

class (Monad m) => MonadService m where
  createService :: Text -> Service -> m Service
  listService :: Text -> Maybe Text -> m ServiceList
  getService :: Text -> Text -> m (Maybe Service)
  updateService :: Text -> Service -> m Service
  deleteService :: Text -> Text -> m Status
  waitForService :: Text -> Text -> Text -> (Maybe Service -> Bool) -> m (Maybe Service)

instance (MonadIO m, MonadThrow m, MonadCatch m, MonadConsole m, MonadFileSystem m, MonadWait m, HasConfig c) => MonadService (Resource c m) where

  createService namespace secret = do
    logDebug $ "Creating secret '" <> (decodeUtf8.toStrict.encode) secret <> "'"
    restCall $ createNamespacedService namespace Nothing secret

  listService namespace selector = do
    logDebug $ "List secrets in '" <> namespace <> "'"
    restCall $ listNamespacedService namespace Nothing selector Nothing Nothing Nothing Nothing

  getService namespace name = do
    logDebug $ "Get secret '" <> namespace <> "/" <> name <> "'"
    restCallGetter $ readNamespacedService namespace name Nothing

  updateService namespace secret = do
    logDebug $ "Update secret '" <> (decodeUtf8.toStrict.encode) secret <> "'"
    restCall $ replaceNamespacedService namespace (secret ^. Service.metadata._Just.ObjectMeta.name._Just) Nothing secret

  deleteService namespace name = do
    logDebug $ "Delete secret '" <> namespace <> "/" <> name <> "'"
    restCall $ deleteNamespacedService namespace name Nothing

  waitForService message namespace name predicate = waitFor (WaitConfig (Retry 300) (Seconds 1) message) (getService namespace name) predicate

newService :: Text -> Service
newService serviceName = mkService
           & name .~ serviceName

newServiceSpec :: [ServicePort] -> ServiceSpec
newServiceSpec ports = mkServiceSpec ports

newServicePort :: Text -> Integer -> ServicePort
newServicePort name port = mkServicePort port
                         & ServicePort.name ?~ name

serviceSpec :: Traversal' Service ServiceSpec
serviceSpec = Service.spec.non (newServiceSpec [])

podSelector :: Traversal' Service Object
podSelector = serviceSpec.ServiceSpec.selector.non (Any.Any HashMap.empty).Any.any

servicePort :: Text -> Traversal' Service ServicePort
servicePort name = serviceSpec.ServiceSpec.ports.each.filtered (\p -> p ^. ServicePort.name == Just name)

services :: Traversal' ServiceList [Service]
services = ServiceList.items.non []
