{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module CPI.Kubernetes.Action.DeleteDiskSpec(spec) where

import           Test.Hspec

import           Control.Lens
import qualified CPI.Kubernetes.Base64                              as Base64
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy                               (fromStrict,
                                                                     toStrict)
import           Data.HashMap.Strict                                (HashMap)
import qualified Data.HashMap.Strict                                as HashMap
import           Data.HashSet                                       (HashSet)
import qualified Data.HashSet                                       as HashSet
import           Data.Text                                          (Text)
import           Data.Text.Encoding

import           Kubernetes.Model.V1.Container                      (Container)
import qualified Kubernetes.Model.V1.Container                      as Container
import           Kubernetes.Model.V1.Pod                            (Pod)
import qualified Kubernetes.Model.V1.Pod                            as Pod hiding
                                                                            (status)
import           Kubernetes.Model.V1.PodSpec                        (PodSpec)
import qualified Kubernetes.Model.V1.PodSpec                        as PodSpec
import           Kubernetes.Model.V1.PodStatus                      (PodStatus)
import qualified Kubernetes.Model.V1.PodStatus                      as PodStatus
import           Kubernetes.Model.V1.SecretVolumeSource             (SecretVolumeSource,
                                                                     mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.SecretVolumeSource             as SecretVolumeSource
import           Kubernetes.Model.V1.SecurityContext                (SecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext                as SecurityContext
import           Kubernetes.Model.V1.Volume                         (Volume)
import qualified Kubernetes.Model.V1.Volume                         as Volume
import           Kubernetes.Model.V1.VolumeMount                    (VolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount                    as VolumeMount

import           Control.Exception.Safe
import           Control.Monad.Reader

import           Data.Maybe
import           Data.Semigroup
import           Data.Text                                          (Text)
import qualified Data.Text                                          as Text

import qualified CPI.Base                                           as Base

import           Control.Effect.Stub.Console (HasStdin(..), HasStderr(..), HasStdout(..))
import           Control.Effect.Stub.FileSystem (HasFiles(..))
import           Control.Effect.Stub.Wait (HasWaitCount(..))
import           Control.Effect.Stub

import           CPI.Kubernetes.Action.DeleteDisk
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.PersistentVolumeClaim      (createPersistentVolumeClaim,
                                                                     getPersistentVolumeClaim,
                                                                     newPersistentVolumeClaim,
                                                                     phase,
                                                                     status,
                                                                     waitForPersistentVolumeClaim)
import qualified CPI.Kubernetes.Resource.PersistentVolumeClaim      as PersistentVolumeClaim
import           CPI.Kubernetes.Resource.Secret                     (getSecret)
import qualified CPI.Kubernetes.Resource.Secret                     as Secret
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
spec = describe "deleteDisk" $ do
  let config :: (?agent :: Object) => TestConfig
      config = TestConfig {
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

  context "when PersistentVolumeClaim exists" $ do
    it "should delete a PersistentVolumeClaim" $ do
      let testPVC = newPersistentVolumeClaim "test-" "10Mi"
      void $ runStubT'
               config
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        pvc <- createPersistentVolumeClaim "bosh" testPVC
        _ <- waitForPersistentVolumeClaim "PVC to be bound" "bosh" (pvc ^. name) (\pvc -> pvc ^. _Just.status.phase._Just == "Bound")

        deleteDisk $ Base.DiskId $ pvc ^. name

        maybePod <- getPersistentVolumeClaim "bosh" $ pvc ^. name
        lift $ maybePod `shouldSatisfy` isNothing

  context "when PersistentVolumeClaim does not exist" $ do
    it "should not fail" $ do
      let testPVC = newPersistentVolumeClaim "test-" "10Mi"
      void $ runStubT'
               config
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        deleteDisk $ Base.DiskId $ "does-not-exist"

        maybePod <- getPersistentVolumeClaim "bosh" $ "does-not-exist"
        lift $ maybePod `shouldSatisfy` isNothing
