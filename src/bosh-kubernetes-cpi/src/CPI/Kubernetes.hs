{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Kubernetes (
    module CPI.Kubernetes.Config
  ) where

import qualified CPI.Base                                  as Base

import qualified CPI.Kubernetes.Action.AttachDisk          as AttachDisk
import qualified CPI.Kubernetes.Action.CreateDisk          as CreateDisk
import qualified CPI.Kubernetes.Action.CreateVm            as CreateVm
import qualified CPI.Kubernetes.Action.DeleteDisk          as DeleteDisk
import qualified CPI.Kubernetes.Action.DeleteVm            as DeleteVm
import qualified CPI.Kubernetes.Action.DetachDisk          as DetachDisk
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.VmTypes                    (VmProperties)
import qualified CPI.Kubernetes.VmTypes                    as VmTypes

import CPI.Kubernetes.Resource.Pod (getPod)
import CPI.Kubernetes.Resource.PersistentVolumeClaim (getPersistentVolumeClaim)
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
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.Text.Encoding                        (decodeUtf8,
                                                            encodeUtf8)
import           Data.Yaml                                 as Yaml
import           GHC.Generics
import qualified Servant.Common.BaseUrl                    as Url
import qualified Servant.Common.Req

instance Base.CpiConfiguration Config IO where
  parseConfig :: ByteString -> IO Config
  parseConfig = CPI.Kubernetes.Config.parseConfig

instance Base.MonadCpi Config (Resource Config IO) where
  type VmProperties Config = VmProperties
  createStemcell ::
       FilePath
    -> Base.StemcellProperties
    -> Resource Config IO Base.StemcellId
  createStemcell _ (Base.StemcellProperties properties) = do
    let imageId = properties ^. key "image"._String
    if imageId == "" then
      throwM $ Base.CloudError "Unrecognized stemcell. Should provide an 'image' id."
    else
      pure $ Base.StemcellId imageId
  deleteStemcell ::
       Base.StemcellId
    -> Resource Config IO ()
  deleteStemcell _ = pure ()

  createVm ::
       Base.AgentId
    -> Base.StemcellId
    -> VmProperties
    -> Base.Networks
    -> Base.DiskLocality
    -> Base.Environment
    -> Resource Config IO Base.VmId
  createVm = CreateVm.createVm

  hasVm :: Base.VmId
          -> Resource Config IO Bool
  hasVm (Base.VmId vmId) = do
    config <- ask
    let ns = namespace $ clusterAccess config
    getPod ns vmId >>= pure.isJust

  deleteVm :: Base.VmId
          -> Resource Config IO ()
  deleteVm = DeleteVm.deleteVm

  createDisk :: Integer
    -> Base.DiskProperties
    -> Base.VmId
    -> Resource Config IO Base.DiskId
  createDisk = CreateDisk.createDisk

  hasDisk :: Base.DiskId
          -> Resource Config IO Bool
  hasDisk (Base.DiskId claimId) =  do
    config <- ask
    let ns = namespace $ clusterAccess config
    getPersistentVolumeClaim ns claimId >>= pure.isJust

  deleteDisk :: Base.DiskId
          -> Resource Config IO ()
  deleteDisk diskId = do
    config <- ask
    let ns = namespace $ clusterAccess config
    exists <- getPersistentVolumeClaim ns (Unwrapped diskId) >>= pure.isJust
    when exists $ void $ DeleteDisk.deleteDisk diskId


  attachDisk :: Base.VmId
          -> Base.DiskId
          -> Resource Config IO ()
  attachDisk = AttachDisk.attachDisk

  detachDisk :: Base.VmId
          -> Base.DiskId
          -> Resource Config IO ()
  detachDisk = DetachDisk.detachDisk
