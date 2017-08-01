{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module CPI.Kubernetes.Action.CreateDiskSpec(spec) where

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

import           Control.Monad.Stub.Console
import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.StubMonad

import           Control.Monad.Stub.Wait
import           CPI.Kubernetes.Action.CreateDisk
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.PersistentVolumeClaim      (getPersistentVolumeClaim)
import qualified CPI.Kubernetes.Resource.PersistentVolumeClaim      as PersistentVolumeClaim
import           CPI.Kubernetes.Resource.Pod
import qualified CPI.Kubernetes.Resource.Pod                        as Pod
import           CPI.Kubernetes.Resource.Secret                     (getSecret)
import qualified CPI.Kubernetes.Resource.Secret                     as Secret
import           CPI.Kubernetes.Resource.Service
import qualified CPI.Kubernetes.Resource.Service                    as Service
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

runStubT' :: TestConfig -> KubeState -> StubT TestConfig KubeState NoOutput IO () -> IO ((), KubeState, NoOutput)
runStubT' = runStubT

spec :: Spec
spec = describe "createDisk" $ do
  let config :: (?agent :: Object) => TestConfig
      config = TestConfig {
                 config = Config {
                   clusterAccess = ClusterAccess {
                     namespace = pure "bosh"
                   }
                   , agent = ?agent
                 }
                 , stubConfig = emptyStubConfig
               }
      emptyKube' = emptyKube {
        secrets = HashMap.singleton ("default", "default-token") (Secret.newSecret "default-token")
      }
  let ?agent = HashMap.empty :: HashMap Text Value

  it "should create a PersistentVolumeClaim" $ do
    void $ runStubT'
             config
             emptyKube' {
               images = HashSet.singleton "some-image"
             } $ do
      (Base.DiskId diskId) <- createDisk
                1
                (Base.DiskProperties undefined)
                (Base.VmId "")

      maybePod <- getPersistentVolumeClaim "bosh" diskId
      lift $ maybePod `shouldSatisfy` isJust

  describe "should create a PersistentVolumeClaim" $ do
    it "with a name prefix of 'bosh-disk-'" $ do
      void $ runStubT'
               config
               emptyKube' $ do
                 (Base.DiskId diskId) <- createDisk
                           1
                           (Base.DiskProperties undefined)
                           (Base.VmId "")

                 lift $ Text.unpack diskId `shouldContain` "bosh-disk-"
                 maybePVC <- getPersistentVolumeClaim "bosh" diskId
                 lift $ maybePVC `shouldSatisfy` isJust
                 let Just pvc = maybePVC
                 lift $ (Text.unpack $ pvc ^. name) `shouldContain` "bosh-disk-"

    it "with the specified size" $ do
      void $ runStubT'
               config
               emptyKube' $ do
                 (Base.DiskId diskId) <- createDisk
                           1
                           (Base.DiskProperties undefined)
                           (Base.VmId "")

                 pvc <- fromJust <$> getPersistentVolumeClaim "bosh" diskId
                 lift $ (pvc ^? PersistentVolumeClaim.resourceRequests.at "capacity"._Just) `shouldBe` (Just "1Mi")

    it "and wait for the PersistentVolumeClaim to be 'Bound'" $ do
     void $ runStubT'
              config
              emptyKube' {
                images = HashSet.singleton "test-stemcell"
              } $ do
              (Base.DiskId diskId) <- createDisk
                        1
                        (Base.DiskProperties undefined)
                        (Base.VmId "")

              maybePVC <- getPersistentVolumeClaim "bosh" diskId
              lift $ maybePVC `shouldSatisfy` isJust
              let Just pvc = maybePVC
              lift $ (pvc ^. PersistentVolumeClaim.status.PersistentVolumeClaim.phase._Just) `shouldBe` "Bound"

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message
