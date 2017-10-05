{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module CPI.Kubernetes.Action.DeleteVm(
  deleteVm
) where

import CPI.Kubernetes.Action.Common

import qualified CPI.Base                                 as Base
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata         as Metadata
import           CPI.Kubernetes.Resource.Pod              (MonadPod, deletePod,
                                                           waitForPod)
import qualified CPI.Kubernetes.Resource.Pod              as Pod
import           CPI.Kubernetes.Resource.Secret           (MonadSecret,
                                                           createSecret, deleteSecret, waitForSecret, data',
                                                           newSecret)
import qualified CPI.Kubernetes.Resource.Secret           as Secret
import           CPI.Kubernetes.Resource.Service          (MonadService, getService, listService,
                                                           updateService)
import qualified CPI.Kubernetes.Resource.Service          as Service

import           Kubernetes.Model.V1.Any                  (Any)
import qualified Kubernetes.Model.V1.Any                  as Any
import           Kubernetes.Model.V1.Container            (Container,
                                                           mkContainer)
import qualified Kubernetes.Model.V1.Container            as Container
import           Kubernetes.Model.V1.EmptyDirVolumeSource (EmptyDirVolumeSource, mkEmptyDirVolumeSource)
import qualified Kubernetes.Model.V1.EmptyDirVolumeSource as EmptyDirVolumeSource
import           Kubernetes.Model.V1.ObjectMeta           (ObjectMeta,
                                                           mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta           as ObjectMeta
import           Kubernetes.Model.V1.Pod                  (Pod, mkPod)
import qualified Kubernetes.Model.V1.Pod                  as Pod hiding (status)
import           Kubernetes.Model.V1.PodList              (PodList, mkPodList)
import qualified Kubernetes.Model.V1.PodList              as PodList
import           Kubernetes.Model.V1.PodSpec              (PodSpec, mkPodSpec)
import qualified Kubernetes.Model.V1.PodSpec              as PodSpec
import           Kubernetes.Model.V1.PodStatus            (PodStatus,
                                                           mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus            as PodStatus
import           Kubernetes.Model.V1.Secret               (Secret, mkSecret)
import qualified Kubernetes.Model.V1.Secret               as Secret
import           Kubernetes.Model.V1.Service               (Service, mkService)
import qualified Kubernetes.Model.V1.Service               as Service
import           Kubernetes.Model.V1.SecretList           (SecretList,
                                                           mkSecretList)
import qualified Kubernetes.Model.V1.SecretList           as SecretList
import           Kubernetes.Model.V1.SecretVolumeSource   (SecretVolumeSource,
                                                           mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.SecretVolumeSource   as SecretVolumeSource
import           Kubernetes.Model.V1.SecurityContext      (SecurityContext,
                                                           mkSecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext      as SecurityContext
import           Kubernetes.Model.V1.Volume               (Volume, mkVolume)
import qualified Kubernetes.Model.V1.Volume               as Volume
import           Kubernetes.Model.V1.VolumeMount          (VolumeMount,
                                                           mkVolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount          as VolumeMount

import           Data.ByteString.Lazy                     (toStrict)
import qualified Data.HashMap.Strict                      as HashMap
import           Data.HashMap.Strict                       (HashMap)
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           Data.Text.Encoding

import           Control.Lens
import           Control.Lens.Wrapped
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Aeson.Lens
import           Data.Semigroup
import           Data.Maybe

import           Control.Exception.Safe
import           Control.Monad.FileSystem
import           Data.Aeson
import qualified Data.Aeson                               as Aeson

deleteVm ::
    (  HasConfig c
     , MonadReader c m
     , MonadLog (WithSeverity Text) m
     , MonadFileSystem m
     , MonadPod m
     , MonadService m
     , MonadSecret m
     , MonadCatch m) =>
     Base.VmId
  -> m ()
deleteVm vmId = do
  logDebug $ "Delete VM with id '" <> Unwrapped vmId <> "'"
  config <- asks asConfig
  let ns = config & clusterAccess & namespace
  disassociate (Wrapped (Unwrapped vmId))
  pod <- ignoringNotFound $ deletePod ns (Unwrapped vmId)
  void $ waitForPod
      "Pod to be deleted"
      ns
      (pod ^. _Just.Metadata.name)
      isNothing
  secret <- ignoringNotFound $ deleteSecret ns ("agent-settings-" <> (Unwrapped vmId))
  void $ waitForSecret
      "Secret to be deleted"
      ns
      ("agent-settings-" <> (Unwrapped vmId))
      isNothing

disassociate ::
  (  HasConfig c
   , MonadReader c m
   , MonadLog (WithSeverity Text) m
   , MonadFileSystem m
   , MonadService m) => Base.AgentId -> m ()
disassociate agentId = do
  config <- asks asConfig
  let ns = config & clusterAccess & namespace
  serviceList <- listService ns $ Just $ "bosh.cloudfoundry.org/agent-id=" <> Unwrapped agentId
  let serviceNames = serviceList ^.. Service.services.each.name
  services <- catMaybes <$> forM serviceNames (getService ns)
  let remove label service = service
                             & labels.at label .~ Nothing
                             & Service.podSelector.at label .~ Nothing
  forM_ ((remove "bosh.cloudfoundry.org/agent-id") <$> services) (updateService ns)
