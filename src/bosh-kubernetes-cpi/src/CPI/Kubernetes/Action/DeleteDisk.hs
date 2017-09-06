{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module CPI.Kubernetes.Action.DeleteDisk(
  deleteDisk
) where

import qualified CPI.Base                                 as Base
import           CPI.Kubernetes.Config
import qualified          CPI.Kubernetes.Resource.Metadata         as Metadata
import           CPI.Kubernetes.Resource.PersistentVolumeClaim
import qualified CPI.Kubernetes.VmTypes                   as VmTypes
import           Resource

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

import Control.Applicative
import           Control.Exception.Safe
import           Control.Monad.FileSystem
import           Data.Aeson
import qualified Data.Aeson                               as Aeson
import Data.Maybe
import Servant.Common.Req
import Network.HTTP.Types.Status

deleteDisk ::
    (  HasConfig c
     , MonadCatch m
     , MonadReader c m
     , MonadLog (WithSeverity Text) m
     , MonadFileSystem m
     , MonadPVC m) =>
     Base.DiskId
  -> m ()
deleteDisk diskId = do
  logDebug $ "Delete disk '" <> Unwrapped diskId <> "'"
  config <- asks asConfig
  let ns = config & clusterAccess & namespace
  pvc <- ignoringNotFound $ deletePersistentVolumeClaim ns (Unwrapped diskId)
  waitForPersistentVolumeClaim
    ns
    (pvc ^. _Just.Metadata.name)
    isNothing
  pure ()

ignoringNotFound :: (Monad m, MonadCatch m) => m b -> m (Maybe b)
ignoringNotFound f = do
  let catchNotFound :: (Monad m, MonadCatch m) => SomeException -> m (Maybe b)
      catchNotFound e = case fromException e of
        Just FailureResponse {
            responseStatus = Status {
              statusCode = 404
            }
          , responseContentType = _
          , responseBody = _
        } -> pure Nothing
        _ -> throwM e
  (Just <$> f) `catch` catchNotFound
