{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CPI.Kubernetes (
    module CPI.Kubernetes.Config
  ) where
import qualified CPI.Base                                  as Base
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Http
import qualified CPI.Kubernetes.Model                      as Model
import           CPI.Kubernetes.Networks                   (networks,
                                                            preconfigured)
import qualified CPI.Kubernetes.Secrets                    as Secrets
import qualified CPI.Kubernetes.VmTypes                    as VmTypes
import           Data.Maybe

import           Control.Lens
import           Control.Lens.At
import           Control.Lens.Cons
import           Control.Monad                             (mapM)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Log
import           Control.Monad.Reader
import qualified Data.Aeson                                as Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString                           as BS
import qualified Data.ByteString.Base64                    as Base64
import           Data.ByteString.Lazy                      (fromStrict,
                                                            toStrict)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as HashMap
import           Data.Semigroup
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.Text.Encoding                        (decodeUtf8,
                                                            encodeUtf8)
import           Data.Yaml                                 as Yaml
import           GHC.Generics
import qualified Servant.Common.BaseUrl                    as Url
import qualified Servant.Common.Req

import qualified CPI.Kubernetes.Action.AttachDisk          as AttachDisk
import qualified CPI.Kubernetes.Action.CreateDisk          as CreateDisk
import qualified CPI.Kubernetes.Action.CreateVm            as CreateVm
import qualified CPI.Kubernetes.Action.DeleteDisk          as DeleteDisk
import qualified CPI.Kubernetes.Action.DeleteVm            as DeleteVm
import qualified CPI.Kubernetes.Action.DetachDisk          as DetachDisk
import           Resource

import qualified Kubernetes.Api.ApivApi                    as Kube
import qualified Kubernetes.Model.Unversioned.Status       as Status
import qualified Kubernetes.Model.V1.Any                   as Any
import qualified Kubernetes.Model.V1.Container             as Container
import qualified Kubernetes.Model.V1.ObjectMeta            as ObjectMeta
import qualified Kubernetes.Model.V1.PersistentVolumeClaim as PersistentVolumeClaim
import qualified Kubernetes.Model.V1.Pod                   as Pod
import qualified Kubernetes.Model.V1.PodSpec               as PodSpec
import qualified Kubernetes.Model.V1.ResourceRequirements  as ResourceRequirements
import qualified Kubernetes.Model.V1.Secret                as Secret
import qualified Kubernetes.Model.V1.SecretList            as SecretList
import qualified Kubernetes.Model.V1.SecurityContext       as SecurityContext
import qualified Kubernetes.Model.V1.Service               as Service
import qualified Kubernetes.Model.V1.ServiceList           as ServiceList
import qualified Kubernetes.Model.V1.ServicePort           as ServicePort
import qualified Kubernetes.Model.V1.ServiceSpec           as ServiceSpec

instance Base.MonadCpi Config IO where
  parseConfig :: ByteString -> IO Config
  parseConfig = CPI.Kubernetes.Config.parseConfig

  createStemcell ::
       FilePath
    -> Base.StemcellProperties
    -> Base.Cpi Config IO Base.StemcellId
  createStemcell _ (Base.StemcellProperties properties) = do
    let imageId = properties ^. key "image"._String
    if imageId == "" then
      throwM $ Base.CloudError "Unrecognized stemcell. Should provide an 'image' id."
    else
      pure $ Base.StemcellId imageId
  deleteStemcell ::
       Base.StemcellId
    -> Base.Cpi Config IO ()
  deleteStemcell _ = pure ()

  createVm ::
       Base.AgentId
    -> Base.StemcellId
    -> Base.VmProperties
    -> Base.Networks
    -> Base.DiskLocality
    -> Base.Environment
    -> Base.Cpi Config IO Base.VmId
  createVm agentId stemcell cloudProperties networkSpec diskLocality env = do
    logDebug $ "Create VM for agent '" <> Unwrapped agentId <> "'"
    config <- ask
    config `runResource` CreateVm.createVm agentId stemcell cloudProperties networkSpec diskLocality env

  hasVm :: Base.VmId
          -> Base.Cpi Config IO Bool
  hasVm (Base.VmId vmId) = hasPod vmId

  deleteVm :: Base.VmId
          -> Base.Cpi Config IO ()
  deleteVm vmId = do
    logDebug $ "Delete VM with id '" <> Unwrapped vmId <> "'"
    config <- ask
    config `runResource` DeleteVm.deleteVm vmId

  createDisk :: Integer
    -> Base.DiskProperties
    -> Base.VmId
    -> Base.Cpi Config IO Base.DiskId
  createDisk size properties vmId = do
    logDebug $
         "Creating disk with size '" <> Text.pack (show size)
      <> "', properties '" <> Text.pack (show properties)
      <> "' for VM '" <> (Unwrapped vmId) <> "'"
    config <- ask
    config `runResource` CreateDisk.createDisk size properties vmId

  hasDisk :: Base.DiskId
          -> Base.Cpi Config IO Bool
  hasDisk (Base.DiskId claimId) = hasPersistentVolumeClaim claimId

  deleteDisk :: Base.DiskId
          -> Base.Cpi Config IO ()
  deleteDisk diskId = do
    logDebug $ "Delete disk '" <> Text.pack (show (Unwrapped diskId)) <> "'"
    exists <- hasPersistentVolumeClaim (Unwrapped diskId)
    config <- ask
    when exists $ void $ config `runResource` DeleteDisk.deleteDisk diskId


  attachDisk :: Base.VmId
          -> Base.DiskId
          -> Base.Cpi Config IO ()
  attachDisk vmId diskId = do
    config <- ask
    config `runResource` AttachDisk.attachDisk vmId diskId

  detachDisk :: Base.VmId
          -> Base.DiskId
          -> Base.Cpi Config IO ()
  detachDisk vmId diskId =  do
    config <- ask
    config `runResource` DetachDisk.detachDisk vmId diskId
