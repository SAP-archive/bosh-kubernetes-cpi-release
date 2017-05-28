{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Kubernetes.Resource.Stub.Pod() where

import           Kubernetes.Model.V1.Container          (Container, mkContainer)
import qualified Kubernetes.Model.V1.Container          as Container
import           Kubernetes.Model.V1.DeleteOptions      (mkDeleteOptions)
import           Kubernetes.Model.V1.ObjectMeta         (ObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta         as ObjectMeta
import           Kubernetes.Model.V1.Pod                (Pod)
import qualified Kubernetes.Model.V1.Pod                as Pod
import           Kubernetes.Model.V1.PodList            (PodList)
import qualified Kubernetes.Model.V1.PodList            as PodList
import           Kubernetes.Model.V1.PodSpec            (PodSpec)
import qualified Kubernetes.Model.V1.PodSpec            as PodSpec
import           Kubernetes.Model.V1.PodStatus          (PodStatus, mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus          as PodStatus
import           Kubernetes.Model.V1.SecretVolumeSource (SecretVolumeSource,
                                                         mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.SecretVolumeSource as SecretVolumeSource
import           Kubernetes.Model.V1.Volume             (Volume, mkVolume)
import qualified Kubernetes.Model.V1.Volume             as Volume
import           Kubernetes.Model.V1.VolumeMount        (VolumeMount,
                                                         mkVolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount        as VolumeMount

import           Kubernetes.Api.ApivApi                 (createNamespacedPod,
                                                         deleteNamespacedPod,
                                                         listNamespacedPod,
                                                         readNamespacedPod,
                                                         replaceNamespacedPod)

import qualified Control.Monad.State                    as State
import           Data.Hourglass
import           Data.Maybe
import           Data.Text                              (Text)

import           Control.Exception.Safe
import           Control.Lens
import           Network.HTTP.Types.Status
import           Servant.Client

import           CPI.Kubernetes.Resource.Pod

import           Control.Monad.Stub.Console
import           Control.Monad.Stub.StubMonad
import           Control.Monad.Stub.Time
import           Control.Monad.Stub.Wait
import           CPI.Kubernetes.Resource.Stub.State     (HasImages (..),
                                                         HasPods (..))

import           Control.Monad.Time
import           Control.Monad.Wait

import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as HashMap

instance (MonadThrow m, Monoid w, HasPods s, HasWaitCount w, HasTime s, HasTimeline s) => MonadPod (StubT r s w m) where

  createPod namespace pod = do
    let podName = pod ^. name
    pods <- State.gets asPods
    if isJust $ HashMap.lookup (namespace, podName) pods
      then throwM FailureResponse {
        responseStatus = Status {
            statusCode = 409
        }
      }
      else pure ()
    let pod' = (`status` "Pending") $ defaultServiceAccount pod
        defaultServiceAccount :: Pod -> Pod
        defaultServiceAccount pod =
          let
            volume = Volume.mkVolume "default-token"
                   & Volume.secret .~ Just secretVolume
            secretVolume = mkSecretVolumeSource
                         & SecretVolumeSource.secretName .~ Just "default-token"
            volumeMount = mkVolumeMount "default-token" "/var/run/secrets/kubernetes.io/serviceaccount"
                        & VolumeMount.readOnly .~ Just True
            in pod
             & Pod.spec._Just.PodSpec.serviceAccountName .~ Just namespace
             & Pod.spec._Just.PodSpec.volumes.non [] %~ (\volumes -> volume <| volumes)
             & container.Container.volumeMounts.non [] %~ (\mounts -> volumeMount <| mounts)
        status :: Pod -> Text -> Pod
        status pod status = pod
          & Pod.status.non mkPodStatus.PodStatus.phase .~ Just status
    let pods' = HashMap.insert (namespace, podName) pod' pods
    State.modify $ updatePods pods'
    timestamp <- currentTime
    State.modify $ withTimeline
                 (\events ->
                   HashMap.insert (timestamp + (Elapsed $ Seconds 1))
                   [withPods $ HashMap.adjust (\pod -> status pod "Running") (namespace, podName)]
                   events)
    pure pod'

  listPod namespace = do
    kube <- State.get
    pure undefined

  getPod namespace name = do
    pods <- State.gets asPods
    pure $ HashMap.lookup (namespace, name) pods

  updatePod namespace pod = do
    pods <- State.gets asPods
    State.put undefined
    pure undefined

  deletePod namespace name = do
    pods <- State.gets asPods
    State.put undefined
    pure undefined

  waitForPod namespace name predicate = waitFor (WaitConfig (Retry 20) (Seconds 1)) (getPod namespace name) predicate
