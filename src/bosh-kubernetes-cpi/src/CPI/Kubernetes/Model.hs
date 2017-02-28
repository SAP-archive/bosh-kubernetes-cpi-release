{-# LANGUAGE OverloadedStrings #-}
module CPI.Kubernetes.Model where


import           Control.Lens

import           Data.Aeson
import           Data.HashMap.Strict                                   (HashMap)
import qualified Data.HashMap.Strict                                   as HashMap
import           Data.Text                                             (Text)
import qualified Data.Text                                             as Text
import qualified Kubernetes.Model.V1.Any                               as Any
import qualified Kubernetes.Model.V1.Container                         as Container
import qualified Kubernetes.Model.V1.EmptyDirVolumeSource              as EmptyDirVolumeSource
import qualified Kubernetes.Model.V1.ObjectMeta                        as ObjectMeta
import qualified Kubernetes.Model.V1.PersistentVolumeAccessMode        as PersistentVolumeAccessMode
import           Kubernetes.Model.V1.PersistentVolumeClaim             (PersistentVolumeClaim)
import qualified Kubernetes.Model.V1.PersistentVolumeClaim             as PersistentVolumeClaim
import           Kubernetes.Model.V1.PersistentVolumeClaimSpec         (PersistentVolumeClaimSpec)
import qualified Kubernetes.Model.V1.PersistentVolumeClaimSpec         as PersistentVolumeClaimSpec
import qualified Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource as PersistentVolumeClaimVolumeSource
import qualified Kubernetes.Model.V1.Pod                               as Pod
import qualified Kubernetes.Model.V1.PodSpec                           as PodSpec
import           Kubernetes.Model.V1.ResourceRequirements              (ResourceRequirements)
import qualified Kubernetes.Model.V1.ResourceRequirements              as ResourceRequirements
import qualified Kubernetes.Model.V1.Secret                            as Secret
import qualified Kubernetes.Model.V1.SecretVolumeSource                as SecretVolumeSource
import qualified Kubernetes.Model.V1.SecurityContext                   as SecurityContext
import qualified Kubernetes.Model.V1.Service                           as Service
import qualified Kubernetes.Model.V1.ServiceSpec                       as ServiceSpec
import qualified Kubernetes.Model.V1.Volume                            as Volume
import qualified Kubernetes.Model.V1.VolumeMount                       as VolumeMount

import qualified CPI.Base                                              as Base



secretVolume :: Text -> Text -> Volume.Volume
secretVolume volumeName secretName = Volume.mkVolume volumeName
                & Volume.secret .~ Just (SecretVolumeSource.mkSecretVolumeSource & SecretVolumeSource.secretName .~ Just secretName)

emptyVolume :: Text -> Volume.Volume
emptyVolume volumeName = Volume.mkVolume volumeName
                & Volume.emptyDir .~ Just EmptyDirVolumeSource.mkEmptyDirVolumeSource

persistentVolume :: Text -> Text -> Volume.Volume
persistentVolume volumeName claimName = Volume.mkVolume volumeName
  & Volume.persistentVolumeClaim .~ Just (PersistentVolumeClaimVolumeSource.mkPersistentVolumeClaimVolumeSource claimName)

volumeMount :: Text -> Text -> Bool -> VolumeMount.VolumeMount
volumeMount name path readOnly = let
  maybeReadOnly = if readOnly then Just True else Nothing
  in VolumeMount.mkVolumeMount name path
                  & VolumeMount.readOnly .~ maybeReadOnly

container :: Text -> Base.StemcellId -> Container.Container
container name (Base.StemcellId stemcell) = Container.mkContainer name
                & Container.image .~ Just stemcell

podSpec :: Container.Container -> PodSpec.PodSpec
podSpec container = PodSpec.mkPodSpec [container]

pod :: Text -> PodSpec.PodSpec -> Pod.Pod
pod prefix podSpec = let
  metadata = ObjectMeta.mkObjectMeta
      & ObjectMeta.generateName .~ Just prefix
  in Pod.mkPod
      & Pod.metadata .~ Just metadata
      & Pod.spec .~ Just podSpec

cpPod :: Pod.Pod -> Pod.Pod
cpPod pod = let
  name = pod ^? Pod.metadata._Just.ObjectMeta.name._Just
  labels = pod ^? Pod.metadata._Just.ObjectMeta.labels._Just
  annotations = pod ^? Pod.metadata._Just.ObjectMeta.annotations._Just
  metadata = ObjectMeta.mkObjectMeta
      & ObjectMeta.name .~ name
      & ObjectMeta.labels .~ labels
      & ObjectMeta.annotations .~ annotations
  in pod & Pod.metadata .~ Just metadata

service :: Text -> ServiceSpec.ServiceSpec -> Service.Service
service prefix serviceSpec = let
  metadata = ObjectMeta.mkObjectMeta
      & ObjectMeta.generateName .~ Just prefix
  in Service.mkService
      & Service.metadata .~ Just metadata
      & Service.spec .~ Just serviceSpec

persistentVolumeClaim :: Text -> Text -> PersistentVolumeClaim
persistentVolumeClaim prefix size = let
  metadata = ObjectMeta.mkObjectMeta
      & ObjectMeta.generateName .~ Just prefix
  spec = persistentVolumeClaimSpec
       & PersistentVolumeClaimSpec.resources
       .~ Just resourceRequirements
       & PersistentVolumeClaimSpec.accessModes
       .~ Just [PersistentVolumeAccessMode.mkPersistentVolumeAccessMode]
  resourceRequirements = ResourceRequirements.mkResourceRequirements
                       & ResourceRequirements.requests
                       .~ (Just $ Any.Any $ HashMap.fromList [
                              ("capacity", String size)
                            , ("storage", String size)
                          ])
  in
    PersistentVolumeClaim.mkPersistentVolumeClaim
      & PersistentVolumeClaim.metadata .~ Just metadata
      & PersistentVolumeClaim.spec .~ Just spec

persistentVolumeClaimSpec :: PersistentVolumeClaimSpec
persistentVolumeClaimSpec = PersistentVolumeClaimSpec.mkPersistentVolumeClaimSpec
