{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module CPI.Kubernetes.Action.DetachDiskSpec(spec) where

import           Test.Hspec

import           Control.Lens
import           CPI.Base.AgentConfig
import qualified CPI.Kubernetes.Base64                                 as Base64
import CPI.Kubernetes.VmTypes (VmProperties, emptyVmProperties)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy                                  (fromStrict,
                                                                        toStrict)
import           Data.HashMap.Strict                                   (HashMap)
import qualified Data.HashMap.Strict                                   as HashMap
import           Data.HashSet                                          (HashSet)
import qualified Data.HashSet                                          as HashSet
import           Data.Text                                             (Text)
import           Data.Text.Encoding

import           Kubernetes.Model.V1.Container                         (Container)
import qualified Kubernetes.Model.V1.Container                         as Container
import           Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource (PersistentVolumeClaimVolumeSource,
                                                                        mkPersistentVolumeClaimVolumeSource)
import qualified Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource as PersistentVolumeClaimVolumeSource
import           Kubernetes.Model.V1.Pod                               (Pod)
import qualified Kubernetes.Model.V1.Pod                               as Pod hiding
                                                                               (status)
import           Kubernetes.Model.V1.PodSpec                           (PodSpec)
import qualified Kubernetes.Model.V1.PodSpec                           as PodSpec
import           Kubernetes.Model.V1.PodStatus                         (PodStatus)
import qualified Kubernetes.Model.V1.PodStatus                         as PodStatus
import           Kubernetes.Model.V1.SecurityContext                   (SecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext                   as SecurityContext
import           Kubernetes.Model.V1.Volume                            (Volume)
import qualified Kubernetes.Model.V1.Volume                            as Volume
import           Kubernetes.Model.V1.VolumeMount                       (VolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount                       as VolumeMount

import           Control.Exception.Safe
import           Control.Monad.Reader

import           Data.Maybe
import           Data.Semigroup

import qualified CPI.Base                                              as Base

import           Control.Effect.Stub.Console (HasStdin(..), HasStderr(..), HasStdout(..))
import           Control.Effect.Stub.FileSystem (HasFiles(..))
import           Control.Effect.Stub.Wait (HasWaitCount(..))
import           Control.Effect.Stub

import           CPI.Kubernetes.Action.AttachDisk
import           CPI.Kubernetes.Action.CreateDisk
import           CPI.Kubernetes.Action.CreateVm
import           CPI.Kubernetes.Action.DetachDisk
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Pod
import qualified CPI.Kubernetes.Resource.Pod                           as Pod
import           CPI.Kubernetes.Resource.Secret                        (data', deleteSecret,
                                                                        getSecret,
                                                                        waitForSecret)
import qualified CPI.Kubernetes.Resource.Secret                        as Secret
import           CPI.Kubernetes.Resource.Service                       (getService)
import qualified CPI.Kubernetes.Resource.Service                       as Service
import           CPI.Kubernetes.Resource.Stub.PersistentVolumeClaim
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.Service
import           CPI.Kubernetes.Resource.Stub.State
import           Data.Aeson.QQ
import           Resource

instance HasStdin TestConfig where
  asStdin = const ""

data TestConfig = TestConfig {
    config     :: Config
  , stubConfig :: StubConfig
}

instance HasConfig TestConfig where
  asConfig = config

runStubT' :: TestConfig -> KubeState -> StubT TestConfig NoOutput KubeState IO () -> IO ((), KubeState, NoOutput)
runStubT' = runStubT

spec :: Spec
spec = describe "detachVm" $ do
  let agentId = "test-agent"
      prepare = do
        vmId <- createVm
                  (Base.AgentId agentId)
                  (Base.StemcellId "some-image")
                  emptyVmProperties
                  (Base.Networks HashMap.empty)
                  [Base.VolumeId ""]
                  (Base.Environment HashMap.empty)
        diskId <- createDisk
                  1
                  (Base.DiskProperties undefined)
                  (Base.VmId "")
        attachDisk vmId diskId
        pure (vmId, diskId)
      access :: (?agent :: Object) => TestConfig
      access = TestConfig {
                 config = Config {
                   clusterAccess = ClusterAccess {
                     namespace = "bosh"
                   }
                   , agent = ?agent
                 }
                 , stubConfig = emptyStubConfig
               }
      emptyKube' = emptyKube {
        secrets = HashMap.singleton ("default", "default-token") (Secret.newSecret "default-token")
      }
  let ?agent = HashMap.empty :: HashMap Text Value

  it "should re-create Pod" $ do
    void $ runStubT'
             access
             emptyKube' {
               images = HashSet.singleton "some-image"
             } $ do
      (vmId, diskId) <- prepare

      detachDisk vmId diskId

      maybePod <- getPod "bosh" (Unwrapped vmId)
      lift $ maybePod `shouldSatisfy` isJust

  describe "should re-create Pod" $ do
    it "with volume removed" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        (vmId, diskId) <- prepare

        detachDisk vmId diskId

        maybePod <- getPod "bosh" (Unwrapped vmId)

        let Just pod = maybePod
            volumeNames = pod ^.. (Pod.volumes.each.Volume.name)
            secretVolumes = pod ^.. (Pod.volumes.each.Volume.persistentVolumeClaim._Just)
        lift $ volumeNames `shouldNotContain` ["persistent-disk"]
        lift $ secretVolumes `shouldNotContain` [mkPersistentVolumeClaimVolumeSource $ Unwrapped diskId]

    it "with volume unmounted" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        (vmId, diskId) <- prepare

        detachDisk vmId diskId

        maybePod <- getPod "bosh" (Unwrapped vmId)

        let Just pod = maybePod
            volumeMountNames = pod ^.. Pod.volumeMounts.each.VolumeMount.name
            volumeMountPaths = pod ^.. Pod.volumeMounts.each.VolumeMount.mountPath
        lift $ volumeMountNames `shouldNotContain` ["persistent-disk"]
        lift $ volumeMountPaths `shouldNotContain` ["/var/vcap/bosh/disks/" <> (Unwrapped diskId)]
    it "and wait for the Pod to be running" $ do
     (_, s, _) <- runStubT'
                    access
                    emptyKube' {
                      images = HashSet.singleton "some-image"
                    } $ do
                    (vmId, diskId) <- prepare

                    detachDisk vmId diskId

                    maybePod <- getPod "bosh" (Unwrapped vmId)
                    lift $ maybePod `shouldSatisfy` isJust
                    let Just pod = maybePod
                    lift $ (pod ^. Pod.status.Pod.phase._Just) `shouldBe` "Running"
     (HashMap.size $ events s) `shouldBe` 8 -- pod running, disk bound, pod terminating, pod deleted, pod running, pod terminating, pod deleted, pod running
     pure ()

  it "should remove persistent disk from agent settings" $ do
    void $ runStubT'
             access
             emptyKube' {
               images = HashSet.singleton "some-image"
             } $ do
      (vmId, diskId) <- prepare

      detachDisk vmId diskId

      maybeSecret <- getSecret "bosh" $ "agent-settings-" <> (Unwrapped vmId)
      lift $ maybeSecret `shouldSatisfy` isJust
      let Just secret = maybeSecret
          rawSettings = secret ^. data'.at "config.json".non ""._String
      settings <- Base64.decodeJSON rawSettings
      lift $ settings ^. disks.persistent.at (Unwrapped diskId) `shouldBe` Nothing

  context "when Pod doesn't exist" $ do
    let prepare = do
          diskId <- createDisk
                      1
                      (Base.DiskProperties undefined)
                      (Base.VmId "")
          pure (Base.VmId "my-pod", diskId)
    it "should fail" $ do
      let detachDiskAction = void $ runStubT'
                             access
                             emptyKube' {
                                 images = HashSet.singleton "some-image"
                             } $ do
                        (vmId, diskId) <- prepare

                        detachDisk vmId diskId
      detachDiskAction `shouldThrow` cloudErrorWithMessage "Pod 'my-pod' does not exist"
  context "when PVC doesn't exist" $ do
    let prepare = do
          vmId <- createVm
                    (Base.AgentId agentId)
                    (Base.StemcellId "some-image")
                    emptyVmProperties
                    (Base.Networks HashMap.empty)
                    [Base.VolumeId ""]
                    (Base.Environment HashMap.empty)
          pure (vmId, Base.DiskId "my-disk")
    it "should fail" $ do
      let detachDiskAction = void $ runStubT'
                             access
                             emptyKube' {
                                 images = HashSet.singleton "some-image"
                             } $ do
                        (vmId, diskId) <- prepare

                        detachDisk vmId diskId
      detachDiskAction `shouldThrow` cloudErrorWithMessage "PersistentVolumeClaim 'my-disk' does not exist"
  context "when Secret doesn't exist" $ do
    let prepare = do
          config <- asks asConfig
          let ns = namespace $ clusterAccess config
          vmId <- createVm
                    (Base.AgentId agentId)
                    (Base.StemcellId "some-image")
                    emptyVmProperties
                    (Base.Networks HashMap.empty)
                    [Base.VolumeId ""]
                    (Base.Environment HashMap.empty)
          diskId <- createDisk
                      1
                      (Base.DiskProperties undefined)
                      (Base.VmId "")
          let settingsName = "agent-settings-" <> (Unwrapped vmId)
          deleteSecret ns settingsName
          waitForSecret "Secret to be deleted"  ns settingsName isNothing
          pure (vmId, diskId)
    it "should fail" $ do
      let detachDiskAction = void $ runStubT'
                             access
                             emptyKube' {
                                 images = HashSet.singleton "some-image"
                             } $ do
                        (vmId, diskId) <- prepare

                        detachDisk vmId diskId
      detachDiskAction `shouldThrow` cloudErrorWithMessage ("Secret 'agent-settings-" <> agentId <> "' does not exist")

cloudError :: Selector Base.CloudError
cloudError (Base.CloudError _) = True
cloudError _                   = False

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message
