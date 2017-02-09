{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPI.Kubernetes (
    module CPI.Kubernetes.Config
  ) where
import Data.Maybe
import qualified CPI.Base                       as Base
import CPI.Kubernetes.Config
import CPI.Kubernetes.Http
import qualified CPI.Kubernetes.Model as Model
import qualified CPI.Kubernetes.VmTypes as VmTypes
import qualified CPI.Kubernetes.Secrets as Secrets

import           Control.Lens
import Control.Lens.At
import Control.Lens.Cons
import           Control.Monad (mapM)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Log
import qualified Data.ByteString                as BS
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Encoding             (encodeUtf8)
import           Data.Yaml
import   qualified       Data.Aeson as Aeson
import           Data.Aeson.Types
import           Data.Aeson.Lens
import           GHC.Generics
import qualified Servant.Common.BaseUrl         as Url
import  qualified Servant.Common.Req
import Data.Semigroup
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified Kubernetes.Model.Unversioned.Status as Status
import qualified Kubernetes.Api.ApivApi         as Kube
import qualified Kubernetes.Model.V1.Container  as Container
import qualified Kubernetes.Model.V1.ObjectMeta as ObjectMeta
import qualified Kubernetes.Model.V1.Pod        as Pod
import qualified Kubernetes.Model.V1.PersistentVolumeClaim        as PersistentVolumeClaim
import qualified Kubernetes.Model.V1.Service        as Service
import qualified Kubernetes.Model.V1.ServiceList        as ServiceList
import qualified Kubernetes.Model.V1.ServicePort        as ServicePort
import qualified Kubernetes.Model.V1.ServiceSpec        as ServiceSpec
import qualified Kubernetes.Model.V1.PodSpec    as PodSpec
import qualified Kubernetes.Model.V1.SecurityContext    as SecurityContext
import qualified Kubernetes.Model.V1.Secret    as Secret
import qualified Kubernetes.Model.V1.SecretList    as SecretList
import qualified Kubernetes.Model.V1.Any as Any
import Data.Text.Conversions

-- TODO not sure if this leads to a clearer instantiation story?
-- newtype KubeM c a = KubeM CpiMonad deriving(Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader c, MonadLog Text)
--
-- data Kubernetes = Kubernetes

instance Base.CPI Config where
  parseConfig :: (MonadCatch m) => Text -> m Config
  parseConfig = CPI.Kubernetes.Config.parseConfig

  createStemcell :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
       FilePath
    -> Base.StemcellProperties
    -> m Base.StemcellId
  createStemcell _ _ = return "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent"

  createVm :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
       Base.AgentId
    -> Base.StemcellId
    -> Base.VmProperties
    -> Base.Networks
    -> Base.DiskLocality
    -> Base.Environment
    -> m Base.VmId
  createVm agentId stemcell (Base.VmProperties cloudProperties) networkSpec diskLocality env = do
    logMessage $ "Create VM for agent '" <> agentId <> "'"
    let labels = HashMap.empty
                      & HashMap.insert "agentId" (toJSON agentId)
    config <- ask
    secret <- let
      settings :: Object
      settings = agent config
                 & HashMap.insert "agent_id" (toJSON agentId)
                 & HashMap.insert "networks" (toJSON networkSpec)
                 & HashMap.insert "env" (toJSON env)
      allSettings = merge settings $ agent config
        where
          merge :: Object -> Object -> Object
          merge underlay overlay = underlay <> overlay
      rawSecret = HashMap.singleton "settings.json" (Aeson.String (toText (Base64 (Aeson.encode allSettings))))
      objectMeta = ObjectMeta.mkObjectMeta
           & ObjectMeta.generateName .~ Just "settings"
           & ObjectMeta.labels .~ Just (Any.Any labels)
      secret = Secret.mkSecret
                 & Secret.metadata
                 .~ Just objectMeta
                 & Secret.data_
                 .~ Just (Any.Any rawSecret)
      in createSecret secret
    logMessage "Secret uploaded"
    let
      serviceSelector = HashMap.empty
                        & HashMap.insert "agentId" (toJSON agentId)
      serviceSpec = VmTypes.createServiceSpec cloudProperties
                    & ServiceSpec.selector
                    .~ Just (Any.Any serviceSelector)
      service = Model.service "vip" serviceSpec
              & Service.metadata._Just.ObjectMeta.labels
              .~ Just (Any.Any labels)
      in createService service
    pod <- let
      securityContext = SecurityContext.mkSecurityContext
                        & SecurityContext.privileged .~ Just True
                        & SecurityContext.runAsUser .~ Just 0
      container       = Model.container "bosh" stemcell
                        & Container.command .~ Just [
                             "/bin/bash", "-c",
                             "cp /etc/resolv.conf /etc/resolv.conf.dup; "
                          <> "umount /etc/resolv.conf; "
                          <> "mv /etc/resolv.conf.dup /etc/resolv.conf; "
                          <> "exec env -i /usr/sbin/runsvdir-start"]
                        & Container.tty .~ Just True
                        & Container.stdin .~ Just True
                        & Container.securityContext .~ Just securityContext
                        & Container.volumeMounts .~ Just [Model.volumeMount "settings" "/var/vcap/bosh/secrets" True]
      podSpec         = Model.podSpec container
                        & PodSpec.volumes .~ Just [Model.secretVolume "settings" $ secretName secret]
                        & PodSpec.restartPolicy .~ Just "Never"
      pod             = Model.pod "bosh-vm" podSpec
                        & (Pod.metadata . _Just . ObjectMeta.labels) .~ Just (Any.Any labels)
      in createPod pod
    return $ podName pod
      where
        podName :: Pod.Pod -> Base.VmId
        podName = view (Pod.metadata . _Just . ObjectMeta.name . _Just)
        secretName :: Secret.Secret -> Text
        secretName = view (Secret.metadata . _Just . ObjectMeta.name . _Just)
  hasVm :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
             Text
          -> m Bool
  hasVm = hasPod

  deleteVm :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
             Text
          -> m ()
  deleteVm vmId = do
    result <- getPod vmId
    case result of
      Nothing -> return ()
      Just pod -> do
        _ <- let
          serviceNames :: Traversal' ServiceList.ServiceList Text
          serviceNames = ServiceList.items.each.Service.metadata._Just.ObjectMeta.name._Just
          Just agentId = pod ^? Pod.metadata._Just.ObjectMeta.labels._Just.Any.any.at "agentId"._Just._String
          selector = "agentId" <> "=" <> agentId
          in do
            logMessage $ "listService selector: '" <> selector <> "'"
            services <- listService selector
            logMessage $ "listService result: " <> (Text.pack . show) services
            secretList <- listSecret selector
            logMessage $ "listSecret result: " <> (Text.pack . show) secretList
            let names = services ^.. serviceNames
            logMessage $ "Found services '" <> Text.pack (show names) <> "' using selector '" <> selector <> "'"
            mapM deleteService names
        _ <- deletePod vmId
        return ()

  createDisk :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
       Integer
    -> Base.DiskProperties
    -> Base.VmId
    -> m Base.DiskId
  createDisk size properties vmId = do
    logMessage $
         "Creating disk with size '" <> Text.pack (show size)
      <> "', properties '" <> Text.pack (show properties)
      <> "' for VM '" <> vmId <> "'"
    let annotations = HashMap.empty
                      & HashMap.insert "volume.beta.kubernetes.io/storage-class" (String "slow")
        claim = Model.persistentVolumeClaim "bosh" (Text.pack (show size ++ "Mi"))
              & PersistentVolumeClaim.metadata._Just.ObjectMeta.annotations .~ Just (Any.Any annotations)
    persistentVolumeClaim <- createPersistentVolumeClaim claim
    return $ diskName persistentVolumeClaim
      where
        diskName :: PersistentVolumeClaim.PersistentVolumeClaim -> Base.DiskId
        diskName = view (PersistentVolumeClaim.metadata . _Just . ObjectMeta.name . _Just)

  hasDisk :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
             Base.DiskId
          -> m Bool
  hasDisk = hasPersistentVolumeClaim

  deleteDisk :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
             Base.DiskId
          -> m ()
  deleteDisk diskId = do
    logMessage $ "Delete disk '" <> Text.pack (show diskId) <> "'"
    exists <- hasPersistentVolumeClaim diskId
    when exists $ void $ deletePersistentVolumeClaim diskId

  attachDisk :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
             Base.VmId
          -> Base.DiskId
          -> m ()
  attachDisk vmId diskId = do
    pod <- getPod vmId
    disk <- getPersistentVolumeClaim diskId
    case (pod, disk) of
      (Nothing, Nothing) -> throwM $ Base.CloudError ("Neither Pod '" <> vmId <> "' nor Disk '" <> diskId <> "' exist")
      (Nothing, _) -> throwM $ Base.CloudError ("Pod '" <> vmId <> "' does not exist")
      (_, Nothing) -> throwM $ Base.CloudError ("Disk '" <> diskId <> "' does not exist")
      (Just pod, Just disk) -> let
        Just agentId = pod ^? Pod.metadata._Just.ObjectMeta.labels._Just.Any.any.at "agentId"._Just._String
        selector = "agentId" <> "=" <> agentId
        in do
          logMessage $ "listSecret selector: '" <> selector <> "'"
          secretList <- listSecret selector
          logMessage $ "listSecret result: " <> (Text.pack . show) secretList
          serviceList <- listService selector
          logMessage $ "listService result: " <> (Text.pack . show) serviceList
          let secret = head $ SecretList._items secretList
              rawSettings = secret ^. Secret.data_._Just.Any.any.at "settings.json".non ""._String
          settings <- Secrets.modifySettings (Base.addPersistentDisk diskId ("/var/vcap/bosh/disks/" <> diskId)) rawSettings
          deletePod $ pod ^. Pod.metadata._Just.ObjectMeta.name._Just
          let newSecret = secret & Secret.data_._Just.Any.any .~ HashMap.singleton "settings.json" (String settings)
          updateSecret newSecret
          let newPod = Model.cpPod pod
                           & Pod.spec._Just.PodSpec.volumes.element 0 %~ (\volume -> volume |> Model.persistentVolume "persistent-disk" diskId)
                           & Pod.spec._Just.PodSpec.containers.element 0.Container.volumeMounts._Just %~ (\mounts -> mounts |> Model.volumeMount "persistent-disk" "/var/vcap/store" False)
                           & Pod.status .~ Nothing
          createPod newPod
          return ()

  detachDisk :: (MonadIO m, MonadCatch m, MonadReader Config m, MonadLog Text m) =>
             Base.VmId
          -> Base.DiskId
          -> m ()
  detachDisk vmId diskId = undefined
