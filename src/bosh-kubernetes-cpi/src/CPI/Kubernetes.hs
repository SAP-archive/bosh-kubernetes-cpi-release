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
    let labels = HashMap.empty
                      & HashMap.insert "agentId" (toJSON agentId)
    config <- ask
    secret <- let
      settings :: Object
      settings = agent config
                 & HashMap.insert "agent_id" (toJSON agentId)
                --  TODO no need to fix, will be deleted soon
                --  & HashMap.insert "networks" (toJSON (networkSpec & _Unwrapped.each._Unwrapped._Object.preconfigured .~ Bool True))
                 & HashMap.insert "env" (toJSON env)
                 & HashMap.insert "disks" (Object $ HashMap.singleton "system" (String "/dev/sda")
                                                  & HashMap.insert "persistent" (Object HashMap.empty))
                 & HashMap.insert "vm" (Object $ HashMap.singleton "name" (String "vm-name"))
      allSettings = merge settings $ agent config
        where
          merge :: Object -> Object -> Object
          merge underlay overlay = underlay <> overlay
      rawSecret = HashMap.singleton "settings.json" (Aeson.String (decodeUtf8 $ Base64.encode $ toStrict $ Aeson.encode allSettings))
      objectMeta = ObjectMeta.mkObjectMeta
           & ObjectMeta.generateName .~ Just "settings"
           & ObjectMeta.labels .~ Just (Any.Any labels)
      secret = Secret.mkSecret
                 & Secret.metadata
                 .~ Just objectMeta
                 & Secret.data_
                 .~ Just (Any.Any rawSecret)
      in createSecret secret
    logDebug "Secret uploaded"
    vmType <- VmTypes.parseVmProperties cloudProperties
    let
      serviceSelector = HashMap.empty
                        & HashMap.insert "agentId" (toJSON agentId)
      services = VmTypes.createServices vmType
              & each.Service.metadata._Just.ObjectMeta.labels
              .~ Just (Any.Any labels)
              & each.Service.spec._Just.ServiceSpec.selector
              .~ Just (Any.Any serviceSelector)
      in
        do
          logDebug $ "Create Services" <> (Text.pack.show) services
          createService `mapM` services
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
                          <> "cp /etc/hosts /etc/hosts.dup; "
                          <> "umount /etc/hosts; "
                          <> "mv /etc/hosts.dup /etc/hosts; "
                          <> "exec env -i /usr/sbin/runsvdir-start"]
                        & Container.tty .~ Just True
                        & Container.stdin .~ Just True
                        & Container.securityContext .~ Just securityContext
                        & Container.volumeMounts .~ Just [
                              Model.volumeMount "settings" "/var/vcap/bosh/secrets" True
                            , Model.volumeMount "ephemeral-disk" "/var/vcap/data" False
                        ]
      podSpec         = Model.podSpec container
                        & PodSpec.volumes .~ Just [
                            Model.secretVolume "settings" $ secretName secret
                          , Model.emptyVolume "ephemeral-disk"]
                        & PodSpec.restartPolicy .~ Just "Never"
      pod             = Model.pod "bosh-vm" podSpec
                        & (Pod.metadata . _Just . ObjectMeta.labels) .~ Just (Any.Any labels)
      in createPod pod
    return $ podName pod
      where
        podName :: Pod.Pod -> Base.VmId
        podName pod = Base.VmId $ pod & view (Pod.metadata . _Just . ObjectMeta.name . _Just)
        secretName :: Secret.Secret -> Text
        secretName = view (Secret.metadata . _Just . ObjectMeta.name . _Just)
  hasVm :: Base.VmId
          -> Base.Cpi Config IO Bool
  hasVm (Base.VmId vmId) = hasPod vmId

  deleteVm :: Base.VmId
          -> Base.Cpi Config IO ()
  deleteVm (Base.VmId vmId) = do
    result <- getPod vmId
    case result of
      Nothing -> return ()
      Just pod -> do
        _ <- let
          serviceNames :: Traversal' ServiceList.ServiceList Text
          serviceNames = ServiceList.items._Just.each.Service.metadata._Just.ObjectMeta.name._Just
          Just agentId = pod ^? Pod.metadata._Just.ObjectMeta.labels._Just.Any.any.at "agentId"._Just._String
          selector = "agentId" <> "=" <> agentId
          in do
            logDebug $ "listService selector: '" <> selector <> "'"
            services <- listService selector
            logDebug $ "listService result: " <> (Text.pack . show) services
            secretList <- listSecret selector
            logDebug $ "listSecret result: " <> (Text.pack . show) secretList
            let names = services ^.. serviceNames
            logDebug $ "Found services '" <> Text.pack (show names) <> "' using selector '" <> selector <> "'"
            mapM deleteService names
        _ <- deletePod vmId
        return ()

  createDisk :: Integer
    -> Base.DiskProperties
    -> Base.VmId
    -> Base.Cpi Config IO Base.DiskId
  createDisk size properties (Base.VmId vmId) = do
    logDebug $
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
        diskName claim = Base.DiskId $ claim & view (PersistentVolumeClaim.metadata . _Just . ObjectMeta.name . _Just)

  hasDisk :: Base.DiskId
          -> Base.Cpi Config IO Bool
  hasDisk (Base.DiskId claimId) = hasPersistentVolumeClaim claimId

  deleteDisk :: Base.DiskId
          -> Base.Cpi Config IO ()
  deleteDisk (Base.DiskId diskId) = do
    logDebug $ "Delete disk '" <> Text.pack (show diskId) <> "'"
    exists <- hasPersistentVolumeClaim diskId
    when exists $ void $ deletePersistentVolumeClaim diskId

  attachDisk :: Base.VmId
          -> Base.DiskId
          -> Base.Cpi Config IO ()
  attachDisk (Base.VmId vmId) (Base.DiskId diskId) = do
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
          logDebug $ "listSecret selector: '" <> selector <> "'"
          secretList <- listSecret selector
          logDebug $ "listSecret result: " <> (Text.pack . show) secretList
          serviceList <- listService selector
          logDebug $ "listService result: " <> (Text.pack . show) serviceList
          let secret = head $ SecretList._items secretList
              rawSettings = secret ^. Secret.data_._Just.Any.any.at "settings.json".non ""._String

          settings <- Secrets.withBase64 (\raw -> do
                agentSettings <- Base.parseSettings raw
                pure $ toStrict $ Aeson.encode $ Base.addPersistentDisk agentSettings diskId ("/var/vcap/bosh/disks/" <> diskId)
              ) rawSettings
          deletePod $ pod ^. Pod.metadata._Just.ObjectMeta.name._Just
          let newSecret = secret & Secret.data_._Just.Any.any .~ HashMap.singleton "settings.json" (String settings)
          updateSecret newSecret
          let newPod = Model.cpPod pod
                           & Pod.spec._Just.PodSpec.volumes.element 0 %~ (\volume -> volume |> Model.persistentVolume "persistent-disk" diskId)
                           & Pod.spec._Just.PodSpec.containers.element 0.Container.volumeMounts._Just %~ (\mounts -> mounts |> Model.volumeMount "persistent-disk" ("/var/vcap/bosh/disks/" <> diskId) False)
                           & Pod.status .~ Nothing
          createPod newPod
          return ()

  detachDisk :: Base.VmId
          -> Base.DiskId
          -> Base.Cpi Config IO ()
  detachDisk (Base.VmId vmId) (Base.DiskId diskId) =  do
    pod <- getPod vmId
    disk <- getPersistentVolumeClaim diskId
    case (pod, disk) of
      (Nothing, Nothing) -> throwM $ Base.CloudError ("Neither Pod '" <> vmId <> "' nor Disk '" <> diskId <> "' exist")
      (Nothing, _) -> throwM $ Base.CloudError ("Pod '" <> vmId <> "' does not exist")
      (_, Nothing) -> throwM $ Base.CloudError ("Disk '" <> diskId <> "' does not exist")
      (Just pod, Just disk) -> let
        Just agentId = pod ^? Pod.metadata._Just.ObjectMeta.labels._Just.Any.any.at "agentId"._Just._String
        selector = "agentId" <> "=" <> agentId
        removeFirst []     = []
        removeFirst (x:xs) = xs
        in do
          secretList <- listSecret selector
          serviceList <- listService selector
          let secret = head $ SecretList._items secretList
              rawSettings = secret ^. Secret.data_._Just.Any.any.at "settings.json".non ""._String

          settings <- Secrets.withBase64 (\raw -> do
                agentSettings <- Base.parseSettings raw
                pure $ toStrict $ Aeson.encode $ Base.removePersistentDisk agentSettings diskId
              ) rawSettings
          deletePod $ pod ^. Pod.metadata._Just.ObjectMeta.name._Just
          let newSecret = secret & Secret.data_._Just.Any.any .~ HashMap.singleton "settings.json" (String settings)
          updateSecret newSecret
          let newPod = Model.cpPod pod
                           & Pod.spec._Just.PodSpec.volumes._Just %~ removeFirst
                           & Pod.spec._Just.PodSpec.containers.element 0.Container.volumeMounts._Just %~ removeFirst
                           & Pod.status .~ Nothing
          createPod newPod
          return ()
