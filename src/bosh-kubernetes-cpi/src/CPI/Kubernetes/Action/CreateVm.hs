{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module CPI.Kubernetes.Action.CreateVm(
  createVm
) where

import  qualified         CPI.Base                            as Base
import           CPI.Kubernetes.Config
import qualified CPI.Kubernetes.Model                as Model
import qualified CPI.Kubernetes.Resource.Pod as Pod
import  CPI.Kubernetes.Resource.Metadata as Metadata
import           CPI.Kubernetes.Resource.Pod (MonadPod, createPod, waitForPod)
import           CPI.Kubernetes.Resource.Secret (MonadSecret, newSecret, data', createSecret)
import  qualified         CPI.Kubernetes.Resource.Secret as Secret
import           Resource

import           Kubernetes.Model.V1.Any             (Any)
import qualified Kubernetes.Model.V1.Any             as Any
import           Kubernetes.Model.V1.Container       (Container, mkContainer)
import qualified Kubernetes.Model.V1.Container       as Container
import           Kubernetes.Model.V1.ObjectMeta      (ObjectMeta, mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta      as ObjectMeta
import           Kubernetes.Model.V1.Pod             (Pod, mkPod)
import qualified Kubernetes.Model.V1.Pod             as Pod hiding (status)
import           Kubernetes.Model.V1.Volume             (Volume, mkVolume)
import qualified Kubernetes.Model.V1.Volume             as Volume
import           Kubernetes.Model.V1.VolumeMount             (VolumeMount, mkVolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount             as VolumeMount
import           Kubernetes.Model.V1.PodStatus             (PodStatus, mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus             as PodStatus
import           Kubernetes.Model.V1.PodList             (PodList, mkPodList)
import qualified Kubernetes.Model.V1.PodList             as PodList
import           Kubernetes.Model.V1.PodSpec         (PodSpec, mkPodSpec)
import qualified Kubernetes.Model.V1.PodSpec         as PodSpec
import           Kubernetes.Model.V1.Secret          (Secret, mkSecret)
import qualified Kubernetes.Model.V1.SecretVolumeSource          as SecretVolumeSource
import           Kubernetes.Model.V1.SecretVolumeSource          (SecretVolumeSource, mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.Secret          as Secret
import           Kubernetes.Model.V1.SecretList          (SecretList, mkSecretList)
import qualified Kubernetes.Model.V1.SecretList          as SecretList
import           Kubernetes.Model.V1.SecurityContext (SecurityContext,
                                                      mkSecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext as SecurityContext

import qualified Data.HashMap.Strict                 as HashMap
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import Data.Text.Encoding
import qualified CPI.Kubernetes.Base64 as Base64
import Data.ByteString.Lazy (toStrict)

import           Control.Monad.Log
import Control.Monad.Reader
import Control.Lens
import Data.Aeson.Lens
import Data.Semigroup

import           Data.Aeson
import qualified Data.Aeson                          as Aeson
import Control.Exception.Safe
import Control.Monad.FileSystem

createVm ::
    (  HasConfig c
     , MonadReader c m
     , MonadLog (WithSeverity Text) m
     , MonadFileSystem m
     , MonadPod m
     , MonadSecret m) =>
     Base.AgentId
  -> Base.StemcellId
  -> Base.VmProperties
  -> Base.Networks
  -> Base.DiskLocality
  -> Base.Environment
  -> m Base.VmId
createVm agentId stemcell cloudProperties networks diskLocality env = do
  logDebug $ "Create VM for agent '" <> Unwrapped agentId <> "'"
  let labels = HashMap.empty
                    & HashMap.insert "bosh.cloudfoundry.org/agent-id" (toJSON agentId)
  config <- asks asConfig
  namespace <- config & clusterAccess & namespace
  secret <- let
    secret = newSecret ("agent-settings-" <> Unwrapped agentId)
             & Metadata.labels .~ labels
             & data' .~ HashMap.singleton "settings.json" (toJSON $ Base64.encodeJSON settings)
    settings = Base.initialAgentSettings agentId networks blobstore env ntp mbus
    blobstore = agent config ^? at "blobstore"._Just._JSON
    ntp = agent config ^. at "ntp"._Just._JSON
    mbus = agent config ^. at "mbus"._Just._String
    in createSecret namespace secret
  pod <- let
    securityContext = mkSecurityContext
                      & SecurityContext.privileged ?~ True
                      & SecurityContext.runAsUser ?~ 0
    container       = Pod.newContainer "bosh" (Unwrapped stemcell)
                    & Container.volumeMounts.non [] %~ (settingsVolumeMount <|)
    settingsVolume  = mkVolume "agent-settings"
                      & Volume.secret ?~ (mkSecretVolumeSource
                      & SecretVolumeSource.secretName ?~ (secret ^. Metadata.name))
    settingsVolumeMount = mkVolumeMount "agent-settings" "/var/vcap/bosh/settings"
    pod             = Pod.newPod (Unwrapped agentId) container
                      & Metadata.labels .~ labels
                      & Pod.container.Container.securityContext .~ Just securityContext
                      & Pod.volumes %~ (settingsVolume <|)
    in createPod namespace pod
  _ <- waitForPod namespace (Unwrapped agentId) (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
  pure $ podName pod
    where
      podName :: Pod.Pod -> Base.VmId
      podName pod = Base.VmId $ pod ^. Metadata.name
      secretName :: Secret.Secret -> Text
      secretName = view (Secret.metadata . _Just . ObjectMeta.name . _Just)
