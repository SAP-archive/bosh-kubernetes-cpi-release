{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module CPI.Kubernetes.Resource.PersistentVolumeClaim(
    module PersistentVolumeClaim
  , MonadPVC(..)
  , newPersistentVolumeClaim
  , spec
  , resources
  , resourceRequests
  , status
  , phase
) where

import qualified CPI.Base                                        as Base
import           CPI.Base.Errors                                 (CloudError (..))
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata                as Metadata
import           CPI.Kubernetes.Resource.Servant
import           Resource

import qualified Kubernetes.Model.V1.Any                         as Any
import           Kubernetes.Model.V1.Container                   (Container,
                                                                  mkContainer)
import qualified Kubernetes.Model.V1.Container                   as Container
import           Kubernetes.Model.V1.DeleteOptions               (mkDeleteOptions)
import           Kubernetes.Model.V1.ObjectMeta                  (ObjectMeta,
                                                                  mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta                  as ObjectMeta
import           Kubernetes.Model.V1.PersistentVolumeAccessMode  (PersistentVolumeAccessMode,
                                                                  mkPersistentVolumeAccessMode)
import qualified Kubernetes.Model.V1.PersistentVolumeAccessMode  as PersistentVolumeAccessMode
import           Kubernetes.Model.V1.PersistentVolumeClaim       (PersistentVolumeClaim,
                                                                  mkPersistentVolumeClaim)
import qualified Kubernetes.Model.V1.PersistentVolumeClaim       as PersistentVolumeClaim
import           Kubernetes.Model.V1.PersistentVolumeClaimList   (PersistentVolumeClaimList,
                                                                  mkPersistentVolumeClaimList)
import qualified Kubernetes.Model.V1.PersistentVolumeClaimList   as PersistentVolumeClaimList
import           Kubernetes.Model.V1.PersistentVolumeClaimSpec   (PersistentVolumeClaimSpec,
                                                                  mkPersistentVolumeClaimSpec)
import           Kubernetes.Model.V1.PersistentVolumeClaimSpec   (PersistentVolumeClaimSpec,
                                                                  mkPersistentVolumeClaimSpec)
import qualified Kubernetes.Model.V1.PersistentVolumeClaimSpec   as PersistentVolumeClaimSpec
import           Kubernetes.Model.V1.PersistentVolumeClaimStatus (PersistentVolumeClaimStatus,
                                                                  mkPersistentVolumeClaimStatus)
import qualified Kubernetes.Model.V1.PersistentVolumeClaimStatus as PersistentVolumeClaimStatus
import           Kubernetes.Model.V1.PodSpec                     (PodSpec,
                                                                  mkPodSpec)
import qualified Kubernetes.Model.V1.PodSpec                     as PodSpec
import           Kubernetes.Model.V1.PodStatus                   (PodStatus,
                                                                  mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus                   as PodStatus
import           Kubernetes.Model.V1.ResourceRequirements        (ResourceRequirements,
                                                                  mkResourceRequirements)
import           Kubernetes.Model.V1.ResourceRequirements        (ResourceRequirements,
                                                                  mkResourceRequirements)
import qualified Kubernetes.Model.V1.ResourceRequirements        as ResourceRequirements
import qualified Kubernetes.Model.V1.ResourceRequirements        as ResourceRequirements
import           Kubernetes.Model.V1.Volume                      (Volume,
                                                                  mkVolume)
import qualified Kubernetes.Model.V1.Volume                      as Volume

import           Kubernetes.Api.ApivApi                          (createNamespacedPersistentVolumeClaim,
                                                                  deleteNamespacedPersistentVolumeClaim,
                                                                  listNamespacedPersistentVolumeClaim,
                                                                  readNamespacedPersistentVolumeClaim,
                                                                  replaceNamespacedPersistentVolumeClaim)

import           Control.Monad.Reader
import qualified Control.Monad.State                             as State
import           Data.Maybe

import           Control.Exception.Safe
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad.Console
import           Control.Monad.FileSystem
import           Control.Monad.Log
import           Control.Monad.Wait

import           Data.Aeson
import           Data.ByteString.Lazy                            (toStrict)
import           Data.HashMap.Strict                             (HashMap)
import qualified Data.HashMap.Strict                             as HashMap
import           Data.Hourglass
import           Data.Semigroup
import           Data.Text                                       (Text)
import qualified Data.Text                                       as Text
import           Data.Text.Encoding                              (decodeUtf8)
import           Servant.Client

class (Monad m) => MonadPVC m where
  createPersistentVolumeClaim :: Text -> PersistentVolumeClaim -> m PersistentVolumeClaim
  listPersistentVolumeClaim :: Text -> m PersistentVolumeClaimList
  getPersistentVolumeClaim :: Text -> Text -> m (Maybe PersistentVolumeClaim)
  updatePersistentVolumeClaim :: Text -> PersistentVolumeClaim -> m PersistentVolumeClaim
  deletePersistentVolumeClaim :: Text -> Text -> m PersistentVolumeClaim
  waitForPersistentVolumeClaim :: Text -> Text -> Text -> (Maybe PersistentVolumeClaim -> Bool) -> m (Maybe PersistentVolumeClaim)

instance (MonadIO m, MonadThrow m, MonadCatch m, MonadConsole m, MonadFileSystem m, MonadWait m, HasConfig c) => MonadPVC (Resource c m) where

  createPersistentVolumeClaim namespace pvc = do
    logDebug $ "Creating PersistentVolumeClaim '" <> (decodeUtf8.toStrict.encode) pvc <> "'"
    restCall $ createNamespacedPersistentVolumeClaim namespace Nothing pvc

  listPersistentVolumeClaim namespace = do
    logDebug $ "List PersistentVolumeClaims in '" <> namespace <> "'"
    restCall $ listNamespacedPersistentVolumeClaim namespace Nothing Nothing Nothing Nothing Nothing Nothing

  getPersistentVolumeClaim namespace name = do
    logDebug $ "Get PersistentVolumeClaim '" <> namespace <> "/" <> name <> "'"
    restCallGetter $ readNamespacedPersistentVolumeClaim namespace name Nothing Nothing Nothing

  updatePersistentVolumeClaim namespace pvc = do
    logDebug $ "Update PersistentVolumeClaim '" <> (decodeUtf8.toStrict.encode) pvc <> "'"
    restCall $ replaceNamespacedPersistentVolumeClaim namespace (pvc ^. name) Nothing pvc

  deletePersistentVolumeClaim namespace name = do
    logDebug $ "Delete PersistentVolumeClaim '" <> namespace <> "/" <> name <> "'"
    restCall $ deleteNamespacedPersistentVolumeClaim namespace name Nothing (mkDeleteOptions 0)

  waitForPersistentVolumeClaim message namespace name predicate = waitFor (WaitConfig (Retry 300) (Seconds 1) message) (getPersistentVolumeClaim namespace name) predicate

newPersistentVolumeClaim :: Text -> Text -> PersistentVolumeClaim
newPersistentVolumeClaim namePrefix size =
  mkPersistentVolumeClaim
        & Metadata.generateName .~ namePrefix
        & PersistentVolumeClaim.spec .~ (Just $ spec size)
  where
    spec size = mkPersistentVolumeClaimSpec
         & PersistentVolumeClaimSpec.resources .~ Just resourceRequirements
         & PersistentVolumeClaimSpec.accessModes .~ Just [PersistentVolumeAccessMode.mkPersistentVolumeAccessMode]
    resourceRequirements = ResourceRequirements.mkResourceRequirements
                         & ResourceRequirements.requests
                         .~ (Just $ Any.Any $ HashMap.fromList [
                                ("capacity", String size)
                              , ("storage", String size)
                            ])

spec :: Traversal' PersistentVolumeClaim PersistentVolumeClaimSpec
spec = PersistentVolumeClaim.spec.non mkPersistentVolumeClaimSpec

resources :: Traversal' PersistentVolumeClaimSpec ResourceRequirements
resources = PersistentVolumeClaimSpec.resources.non mkResourceRequirements

resourceRequests :: Traversal' PersistentVolumeClaim (HashMap Text Value)
resourceRequests = spec.resources.ResourceRequirements.requests.non (Any.Any HashMap.empty).Any.any

status :: Traversal' PersistentVolumeClaim PersistentVolumeClaimStatus
status = PersistentVolumeClaim.status.non mkPersistentVolumeClaimStatus

phase :: Traversal' PersistentVolumeClaimStatus (Maybe Text)
phase = PersistentVolumeClaimStatus.phase
