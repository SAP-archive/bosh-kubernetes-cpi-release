{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module CPI.Kubernetes.Action.CreateVm(
  createVm
) where

import qualified CPI.Base                                 as Base
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata         as Metadata
import           CPI.Kubernetes.Resource.Pod              (Pods, createPod,
                                                           waitForPod)
import qualified CPI.Kubernetes.Resource.Pod              as Pod
import           CPI.Kubernetes.Resource.Secret           (Secrets,
                                                           createSecret, data',
                                                           newSecret)
import qualified CPI.Kubernetes.Resource.Secret           as Secret
import           CPI.Kubernetes.Resource.Service          (Services, getService,
                                                           updateService)
import qualified CPI.Kubernetes.Resource.Service          as Service
import qualified CPI.Kubernetes.VmTypes                   as VmTypes
import CPI.Kubernetes.VmTypes (VmProperties)
import qualified CPI.Kubernetes.VmPropertiesLens as L

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

import qualified CPI.Kubernetes.Base64                    as Base64
import           Data.ByteString.Lazy                     (toStrict)
import qualified Data.HashMap.Strict                      as HashMap
import  Data.HashMap.Strict                       (HashMap)
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
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
import qualified Data.Aeson                               as Aeson

createVm ::
    (  HasConfig c
     , MonadReader c m
     , MonadLog (WithSeverity Text) m
     , FileSystem m
     , Pods m
     , Services m
     , Secrets m) =>
     Base.AgentId
  -> Base.StemcellId
  -> VmProperties
  -> Base.Networks
  -> Base.DiskLocality
  -> Base.Environment
  -> m Base.VmId
createVm agentId stemcell vmProperties networks diskLocality env = do
  logDebug $ "Create VM for agent '" <> Unwrapped agentId <> "'"
  let labels = HashMap.empty
                    & HashMap.insert "bosh.cloudfoundry.org/agent-id" (toJSON agentId)
  config <- asks asConfig
  let ns = config & clusterAccess & namespace
  secret <- let
    secret = newSecret ("agent-settings-" <> Unwrapped agentId)
             & Metadata.labels .~ labels
             & data' .~ HashMap.singleton "config.json" (toJSON $ Base64.encodeJSON settings)
    preconfiguredNetworks = networks & _Wrapped.each._Wrapped.at "preconfigured" ?~ Bool True
    settings = Base.initialAgentSettings agentId preconfiguredNetworks blobstore env ntp mbus
    blobstore = agent config ^? at "blobstore"._Just._JSON
    ntp = agent config ^. at "ntp"._Just._JSON
    mbus = agent config ^. at "mbus"._Just._String
    in createSecret ns secret
  pod <- let
    securityContext = mkSecurityContext
                      & SecurityContext.privileged ?~ True
                      & SecurityContext.runAsUser ?~ 0
    container       = Pod.newContainer "bosh" (Unwrapped stemcell)
                      & Container.volumeMounts.non [] %~ (settingsVolumeMount <|)
                      & Container.volumeMounts.non [] %~ (ephemeralVolumeMount <|)
                      & Container.command .~ Just [
                             "/bin/bash", "-c",
                             "cp /etc/resolv.conf /etc/resolv.conf.dup; "
                          <> "umount /etc/resolv.conf; "
                          <> "mv /etc/resolv.conf.dup /etc/resolv.conf; "
                          <> "cp /etc/hosts /etc/hosts.dup; "
                          <> "umount /etc/hosts; "
                          <> "mv /etc/hosts.dup /etc/hosts; "
                          <> "cp /etc/hostname /etc/hostname.dup; "
                          <> "umount /etc/hostname; "
                          <> "mv /etc/hostname.dup /etc/hostname; "
                          <> "exec env -i /usr/sbin/runsvdir-start"]
                      & Container.tty .~ Just True
                      & Container.stdin .~ Just True
                      & Pod.resources.Pod.limits .~ (fmap String (vmProperties ^. L.resources.L.limits._Just))
                      & Pod.resources.Pod.requests .~ (fmap String (vmProperties ^. L.resources.L.requests._Just))
    ephemeralVolume = Pod.newEmptyVolume "ephemeral-disk"
    ephemeralVolumeMount = mkVolumeMount "ephemeral-disk" "/var/vcap/data"
    settingsVolume  = Pod.newSecretVolume "agent-settings" (secret ^. Metadata.name)
    settingsVolumeMount = mkVolumeMount "agent-settings" "/var/vcap/bosh/settings-source-file"
    pod             = Pod.newPod (Unwrapped agentId) container
                      & Metadata.labels .~ labels
                      & Pod.container.Container.securityContext .~ Just securityContext
                      & Pod.volumes %~ (settingsVolume <|)
                      & Pod.volumes %~ (ephemeralVolume <|)
    in createPod ns pod
  let services = vmProperties ^. L.services
  services `forM_` (`assignTo` agentId)
  _ <- waitForPod "Pod to be running" ns (Unwrapped agentId) (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
  pure $ Base.VmId $ pod ^. name

assignTo ::
  (  HasConfig c
   , MonadReader c m
   , MonadLog (WithSeverity Text) m
   , FileSystem m
   , Services m) => VmTypes.Service -> Base.AgentId -> m (Maybe Service)
service `assignTo` agentId = do
  config <- asks asConfig
  let ns = config & clusterAccess & namespace
  s <- getService ns $ service ^. L.serviceName
  case s of
    Just s' ->
      let s'' = s'
             & label "bosh.cloudfoundry.org/agent-id" .~ Unwrapped agentId
             & Service.podSelector.at "bosh.cloudfoundry.org/agent-id".non ""._String .~ (Unwrapped agentId)
      in
        Just <$> updateService ns s''
    Nothing -> throwM $ Base.CloudError $ "Service '" <> service ^. L.serviceName <> "' could not be found."
