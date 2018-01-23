{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CPI.Kubernetes.Action.AttachDisk(
  attachDisk
) where

import qualified CPI.Base                                      as Base


import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.PersistentVolumeClaim (MonadPVC, getPersistentVolumeClaim,
                                                                updatePersistentVolumeClaim)
import qualified CPI.Kubernetes.Resource.PersistentVolumeClaim as Service
import           CPI.Kubernetes.Resource.Pod                   (MonadPod,
                                                                createPod,
                                                                deletePod,
                                                                getPod,
                                                                newPodFrom,
                                                                waitForPod)
import qualified CPI.Kubernetes.Resource.Pod                   as Pod
import           CPI.Kubernetes.Resource.Secret                (MonadSecret,
                                                                createSecret,
                                                                data',
                                                                getSecret,
                                                                newSecret,
                                                                updateSecret)
import qualified CPI.Kubernetes.Resource.Secret                as Secret

import           Kubernetes.Model.V1.Any                       (Any)
import qualified Kubernetes.Model.V1.Any                       as Any
import           Kubernetes.Model.V1.Container                 (Container,
                                                                mkContainer)
import qualified Kubernetes.Model.V1.Container                 as Container
import           Kubernetes.Model.V1.EmptyDirVolumeSource      (EmptyDirVolumeSource,
                                                                mkEmptyDirVolumeSource)
import qualified Kubernetes.Model.V1.EmptyDirVolumeSource      as EmptyDirVolumeSource
import           Kubernetes.Model.V1.ObjectMeta                (ObjectMeta,
                                                                mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta                as ObjectMeta
import           Kubernetes.Model.V1.Pod                       (Pod, mkPod)
import qualified Kubernetes.Model.V1.Pod                       as Pod hiding
                                                                       (status)
import           Kubernetes.Model.V1.PodList                   (PodList,
                                                                mkPodList)
import qualified Kubernetes.Model.V1.PodList                   as PodList
import           Kubernetes.Model.V1.PodSpec                   (PodSpec,
                                                                mkPodSpec)
import qualified Kubernetes.Model.V1.PodSpec                   as PodSpec
import           Kubernetes.Model.V1.PodStatus                 (PodStatus,
                                                                mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus                 as PodStatus
import           Kubernetes.Model.V1.Secret                    (Secret,
                                                                mkSecret)
import qualified Kubernetes.Model.V1.Secret                    as Secret
import           Kubernetes.Model.V1.SecretList                (SecretList,
                                                                mkSecretList)
import qualified Kubernetes.Model.V1.SecretList                as SecretList
import           Kubernetes.Model.V1.SecretVolumeSource        (SecretVolumeSource,
                                                                mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.SecretVolumeSource        as SecretVolumeSource
import           Kubernetes.Model.V1.SecurityContext           (SecurityContext, mkSecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext           as SecurityContext
import           Kubernetes.Model.V1.Service                   (Service,
                                                                mkService)
import qualified Kubernetes.Model.V1.Service                   as Service
import           Kubernetes.Model.V1.Volume                    (Volume,
                                                                mkVolume)
import qualified Kubernetes.Model.V1.Volume                    as Volume
import           Kubernetes.Model.V1.VolumeMount               (VolumeMount,
                                                                mkVolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount               as VolumeMount

import qualified CPI.Kubernetes.Base64                         as Base64
import           Data.ByteString.Lazy                          (toStrict)
import           Data.HashMap.Strict                           (HashMap)
import qualified Data.HashMap.Strict                           as HashMap
import           Data.Maybe
import           Data.Text                                     (Text)
import qualified Data.Text                                     as Text
import           Data.Text.Encoding

import           Control.Lens
import           Control.Lens.Wrapped
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Aeson.Lens
import           Data.Semigroup

import           Control.Exception.Safe
import           Control.Effect.Class.FileSystem
import           Data.Aeson
import qualified Data.Aeson                                    as Aeson

attachDisk ::
    (  HasConfig c
     , MonadReader c m
     , MonadLog (WithSeverity Text) m
     , FileSystem m
     , MonadPod m
     , MonadPVC m
     , MonadSecret m) =>
     Base.VmId
  -> Base.DiskId
  -> m ()
attachDisk (Base.VmId vmId) (Base.DiskId diskId) = do
  logDebug $ "Attaching disk '" <> diskId <> "' to VM '" <> vmId <> "'"
  config <- asks asConfig
  let ns = namespace $ clusterAccess config
      expectJust :: (MonadThrow m) => Text -> Text -> m (Maybe a) -> m a
      expectJust prefix name action = do
        let message = prefix <> " '" <> name <> "' does not exist"
        maybeValue <- action
        maybe (throwM $ Base.CloudError message) (pure) maybeValue
      expectPod name action = expectJust "Pod" name action
      expectDisk name action = expectJust "PersistentVolumeClaim" name action
      expectSecret name action = expectJust "Secret" name action
  pod <- expectPod vmId (getPod ns vmId)
  expectDisk diskId (getPersistentVolumeClaim ns diskId)
  secret <- expectSecret ("agent-settings-" <> vmId) (getSecret ns $ "agent-settings-" <> vmId)
  deletePod ns vmId
  waitForPod "Pod to be deleted" ns vmId isNothing
  settings <- Base64.withDecoded
                (Base.addPersistentDisk diskId ("/var/vcap/bosh/disks/" <> diskId))
                (secret ^. Secret.data'.at "config.json"._Just._String)
  let secret' = secret & Secret.data'.at "config.json"._Just._String
              .~ settings
  updateSecret ns secret'
  let volume = Pod.newPersistentVolume "persistent-disk" diskId
      volumeMount = Pod.newVolumeMount "persistent-disk" ("/var/vcap/bosh/disks/" <> diskId) False
      pod' = (newPodFrom pod) & Pod.volumes %~ (\volumes -> volumes |> volume)
                              & Pod.volumeMounts %~ (\mounts -> mounts |> volumeMount)
  createPod ns $ pod'
  void $ waitForPod "Pod to be running" ns vmId Pod.isRunning
