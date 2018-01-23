{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes          #-}

module CPI.Kubernetes.Resource.Pod(
    module Pod
  , MonadPod(..)
  , newPod
  , newPodFrom
  , newContainer
  , container
  , volumes
  , newPersistentVolume
  , newEmptyVolume
  , newSecretVolume
  , volumeMounts
  , newVolumeMount
  , image
  , status
  , phase
  , isRunning
  , resources
  , limits
  , limit
  , requests
  , request
) where

import qualified CPI.Base                                              as Base
import           CPI.Base.Errors                                       (CloudError (..))
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Servant
import           Resource

import qualified Kubernetes.Model.V1.Any                               as Any
import           Kubernetes.Model.V1.Container                         (Container,
                                                                        mkContainer)
import qualified Kubernetes.Model.V1.Container                         as Container
import           Kubernetes.Model.V1.ResourceRequirements                         (ResourceRequirements,
                                                                        mkResourceRequirements)
import qualified Kubernetes.Model.V1.ResourceRequirements                         as ResourceRequirements
import           Kubernetes.Model.V1.DeleteOptions                     (mkDeleteOptions)
import           Kubernetes.Model.V1.EmptyDirVolumeSource              (EmptyDirVolumeSource,
                                                                        mkEmptyDirVolumeSource)
import qualified Kubernetes.Model.V1.EmptyDirVolumeSource              as EmptyDirVolumeSource
import           Kubernetes.Model.V1.ObjectMeta                        (ObjectMeta,
                                                                        mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta                        as ObjectMeta
import           Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource (PersistentVolumeClaimVolumeSource,
                                                                        mkPersistentVolumeClaimVolumeSource)
import qualified Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource as PersistentVolumeClaimVolumeSource
import           Kubernetes.Model.V1.Pod                               (Pod,
                                                                        mkPod)
import qualified Kubernetes.Model.V1.Pod                               as Pod
import           Kubernetes.Model.V1.PodList                           (PodList)
import qualified Kubernetes.Model.V1.PodList                           as PodList
import           Kubernetes.Model.V1.PodSpec                           (PodSpec, mkPodSpec)
import qualified Kubernetes.Model.V1.PodSpec                           as PodSpec
import           Kubernetes.Model.V1.PodStatus                         (PodStatus,
                                                                        mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus                         as PodStatus
import           Kubernetes.Model.V1.SecretVolumeSource                (SecretVolumeSource,
                                                                        mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.SecretVolumeSource                as SecretVolumeSource
import           Kubernetes.Model.V1.Volume                            (Volume,
                                                                        mkVolume)
import qualified Kubernetes.Model.V1.Volume                            as Volume
import           Kubernetes.Model.V1.VolumeMount                       (VolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount                       as VolumeMount

import           Kubernetes.Api.ApivApi                                (createNamespacedPod,
                                                                        deleteNamespacedPod,
                                                                        listNamespacedPod,
                                                                        readNamespacedPod,
                                                                        replaceNamespacedPod)

import           Control.Monad.Reader
import qualified Control.Monad.State                                   as State
import           Data.Maybe

import           Control.Exception.Safe
import           Control.Lens
import           Control.Lens.Operators
import           Control.Effect.Class.Console
import           Control.Effect.Class.FileSystem
import           Control.Effect.Class.Wait

import           Control.Monad.Log

import qualified Data.HashMap.Strict as HashMap
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy                                  (toStrict)
import           Data.Hourglass
import           Data.Semigroup
import           Data.Text                                             (Text)
import qualified Data.Text                                             as Text
import           Data.Text.Encoding                                    (decodeUtf8)
import           Servant.Client

class (Monad m) => MonadPod m where
  createPod :: Text -> Pod -> m Pod
  listPod :: Text -> m PodList
  getPod :: Text -> Text -> m (Maybe Pod)
  updatePod :: Text -> Pod -> m Pod
  deletePod :: Text -> Text -> m Pod
  waitForPod :: Text -> Text -> Text -> (Maybe Pod -> Bool) -> m (Maybe Pod)

instance (MonadIO m, MonadThrow m, MonadCatch m, Console m, FileSystem m, Wait m, HasConfig c) => MonadPod (Resource c m) where

  createPod namespace pod = do
    logDebug $ "Creating pod '" <> (decodeUtf8.toStrict.encode) pod <> "'"
    restCall $ createNamespacedPod namespace Nothing pod

  listPod namespace = do
    logDebug $ "List pods in '" <> namespace <> "'"
    restCall $ listNamespacedPod namespace Nothing Nothing Nothing Nothing Nothing Nothing

  getPod namespace name = do
    logDebug $ "Get pod '" <> namespace <> "/" <> name <> "'"
    restCallGetter $ readNamespacedPod namespace name Nothing Nothing Nothing

  updatePod namespace pod = do
    logDebug $ "Update pod '" <> (decodeUtf8.toStrict.encode) pod <> "'"
    restCall $ replaceNamespacedPod namespace (pod ^. Pod.metadata._Just.ObjectMeta.name._Just) Nothing pod

  deletePod namespace name = do
    logDebug $ "Delete pod '" <> namespace <> "/" <> name <> "'"
    restCall $ deleteNamespacedPod namespace name Nothing (mkDeleteOptions 0)

  waitForPod message namespace name predicate = waitFor (WaitConfig (Retry 300) (Seconds 1) message) (getPod namespace name) predicate

newPod :: Text -> Container -> Pod
newPod name container =
  mkPod & Pod.metadata .~ Just metadata
        & Pod.spec .~ Just spec
  where
    metadata = mkObjectMeta
        & ObjectMeta.name .~ Just name
    spec = mkPodSpec [container]

newPodFrom :: Pod -> Pod
newPodFrom pod =
  mkPod & name     .~ pod ^. name
        & labels   .~ pod ^. labels
        & Pod.spec .~ pod ^. Pod.spec

newContainer :: Text -> Text -> Container
newContainer name imageId =
  mkContainer name & Container.image ?~ imageId

spec :: Traversal' Pod PodSpec
spec = Pod.spec._Just

container :: Traversal' Pod Container
container = spec.PodSpec.containers.ix 0

volumes :: Traversal' Pod [Volume]
volumes = spec.PodSpec.volumes.non []

volumeMounts :: Traversal' Pod [VolumeMount]
volumeMounts = container.Container.volumeMounts.non []

newPersistentVolume :: Text -> Text -> Volume.Volume
newPersistentVolume volumeName claimName = Volume.mkVolume volumeName
  & Volume.persistentVolumeClaim .~ Just (PersistentVolumeClaimVolumeSource.mkPersistentVolumeClaimVolumeSource claimName)

newVolumeMount :: Text -> Text -> Bool -> VolumeMount.VolumeMount
newVolumeMount name path readOnly = let
  maybeReadOnly = if readOnly then Just True else Nothing
  in VolumeMount.mkVolumeMount name path
                  & VolumeMount.readOnly .~ maybeReadOnly

newSecretVolume :: Text -> Text -> Volume.Volume
newSecretVolume volumeName secretName = Volume.mkVolume volumeName
                & Volume.secret .~ Just (mkSecretVolumeSource & SecretVolumeSource.secretName .~ Just secretName)

newEmptyVolume :: Text -> Volume.Volume
newEmptyVolume volumeName = Volume.mkVolume volumeName
                & Volume.emptyDir .~ Just mkEmptyDirVolumeSource

image :: Traversal' Container Text
image = Container.image._Just

status :: Traversal' Pod PodStatus
status = Pod.status.non mkPodStatus

phase :: Traversal' PodStatus (Maybe Text)
phase = PodStatus.phase

isRunning :: Maybe Pod -> Bool
isRunning pod = pod ^. _Just.status.phase._Just == "Running"

resources :: Traversal' Container ResourceRequirements
resources = Container.resources.non mkResourceRequirements

limits :: Traversal' ResourceRequirements Object
limits = ResourceRequirements.limits.non (Any.Any HashMap.empty).Any.any

limit :: Text -> Traversal' ResourceRequirements Text
limit limit = limits.at limit.non ""._String

requests :: Traversal' ResourceRequirements Object
requests = ResourceRequirements.requests.non (Any.Any HashMap.empty).Any.any

request :: Text -> Traversal' ResourceRequirements Text
request request = requests.at request.non ""._String