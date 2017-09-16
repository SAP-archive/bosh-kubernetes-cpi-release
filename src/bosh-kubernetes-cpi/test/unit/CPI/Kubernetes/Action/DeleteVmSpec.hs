{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module CPI.Kubernetes.Action.DeleteVmSpec(spec) where

import           Debug.Trace
import           Test.Hspec

import           Control.Lens
import qualified CPI.Kubernetes.Base64                  as Base64
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy                   (fromStrict, toStrict)
import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as HashMap
import           Data.HashSet                           (HashSet)
import qualified Data.HashSet                           as HashSet
import           Data.Text                              (Text)
import           Data.Text.Encoding

import           Kubernetes.Model.V1.Container          (Container)
import qualified Kubernetes.Model.V1.Container          as Container
import           Kubernetes.Model.V1.Pod                (Pod)
import qualified Kubernetes.Model.V1.Pod                as Pod hiding (status)
import           Kubernetes.Model.V1.PodSpec            (PodSpec)
import qualified Kubernetes.Model.V1.PodSpec            as PodSpec
import           Kubernetes.Model.V1.PodStatus          (PodStatus)
import qualified Kubernetes.Model.V1.PodStatus          as PodStatus
import           Kubernetes.Model.V1.SecretVolumeSource (SecretVolumeSource,
                                                         mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.SecretVolumeSource as SecretVolumeSource
import           Kubernetes.Model.V1.SecurityContext    (SecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext    as SecurityContext
import           Kubernetes.Model.V1.Volume             (Volume)
import qualified Kubernetes.Model.V1.Volume             as Volume
import           Kubernetes.Model.V1.VolumeMount        (VolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount        as VolumeMount

import           Control.Exception.Safe
import           Control.Monad.Reader

import           Data.Maybe
import           Data.Semigroup

import qualified CPI.Base                               as Base

import           Control.Monad.Stub.Console
import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.StubMonad

import           Control.Monad.Stub.Wait
import           CPI.Kubernetes.Action.CreateVm
import           CPI.Kubernetes.Action.DeleteVm
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Pod
import qualified CPI.Kubernetes.Resource.Pod            as Pod
import           CPI.Kubernetes.Resource.Secret         (deleteSecret,
                                                         getSecret,
                                                         waitForSecret)
import qualified CPI.Kubernetes.Resource.Secret         as Secret
import           CPI.Kubernetes.Resource.Service        (createService,
                                                         deleteService,
                                                         getService, newService)
import qualified CPI.Kubernetes.Resource.Service        as Service
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
spec = describe "deleteVm" $ do
  let access :: (?agent :: Object) => TestConfig
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
  let prepare = do
        (Base.VmId vmId) <- createVm
          (Base.AgentId "test-agent")
          (Base.StemcellId "some-image")
          (Base.VmProperties $ Object HashMap.empty)
          (Base.Networks HashMap.empty)
          [Base.VolumeId ""]
          (Base.Environment HashMap.empty)

        lift $ vmId `shouldBe` "test-agent"
        maybePod <- getPod "bosh" vmId
        lift $ maybePod `shouldSatisfy` isJust
        pure $ Base.VmId vmId

  it "should delete the Pod" $ do
    void $ runStubT'
      access
      emptyKube' {
       images = HashSet.singleton "some-image"
      } $ do
        vmId <- prepare

        deleteVm vmId

        maybePod <- getPod "bosh" (Unwrapped vmId)
        lift $ maybePod `shouldSatisfy` isNothing

  describe "should delete a Pod" $ do

    it "and wait for the Pod to be gone" $ do
     void $ runStubT'
          access
          emptyKube' {
            images = HashSet.singleton "some-image"
          } $ do
            vmId <- prepare

            deleteVm vmId

            maybePod <- getPod "bosh" (Unwrapped vmId)
            lift $ maybePod `shouldSatisfy` isNothing

    it "should delete the agent settings Secret" $ do
     void $ runStubT'
          access
          emptyKube' {
            images = HashSet.singleton "some-image"
          } $ do
            vmId <- prepare

            maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
            lift $ maybeSecret `shouldSatisfy` isJust

            deleteVm vmId

            maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
            lift $ maybeSecret `shouldSatisfy` isNothing

    context "when the agent settings secrets don't exist" $ do
      it "should not fail" $ do
        void $ runStubT'
             access
             emptyKube' {
               images = HashSet.singleton "some-image"
             } $ do
               vmId <- prepare
               deleteSecret "bosh" "agent-settings-test-agent"
               waitForSecret "Secret to be deleted" "bosh" "agent-settings-test-agent" isNothing

               deleteVm vmId

    context "when vm cloud_properties specify Services" $ do
      let prepare = do
            service1 <- createService "bosh" $ newService "my-service-1"
            service2 <- createService "bosh" $ newService "my-service-2"
            createVm
              (Base.AgentId "test-agent")
              (Base.StemcellId "some-image")
              (Base.VmProperties $ Object $ HashMap.singleton "services" $ toJSON [Object $ HashMap.singleton "name" "my-service-1", Object $ HashMap.singleton "name" "my-service-2"])
              (Base.Networks HashMap.empty)
              [Base.VolumeId ""]
              (Base.Environment HashMap.empty)
      context "when all Services exists" $ do
        let emptyKube'' =
               emptyKube' {
                   images = HashSet.singleton "some-image"
                 , services = HashMap.empty
               }
        it "should disassociate the Services from the Pod" $ do
          void $ runStubT'
                  access
                  emptyKube'' $ do
                        vmId <- prepare

                        deleteVm vmId

                        maybeService <- getService "bosh" "my-service-1"
                        lift $ maybeService `shouldSatisfy` isJust
                        let Just service = maybeService
                            serviceLabels = service ^. labels
                            podSelector = service ^. Service.podSelector
                        lift $ serviceLabels ^. at "bosh.cloudfoundry.org/agent-id" `shouldBe` Nothing
                        lift $ podSelector ^. at "bosh.cloudfoundry.org/agent-id" `shouldBe` Nothing

                        maybeService <- getService "bosh" "my-service-2"
                        let Just service = maybeService
                            serviceLabels = service ^. labels
                            podSelector = service ^. Service.podSelector
                        lift $ serviceLabels ^. at "bosh.cloudfoundry.org/agent-id" `shouldBe` Nothing
                        lift $ podSelector ^. at "bosh.cloudfoundry.org/agent-id" `shouldBe` Nothing

      context "when Services don't exist" $ do
        let prepare' = do
              vmId <- prepare
              deleteService "bosh" "my-service-1"
              deleteService "bosh" "my-service-2"
              pure vmId
        it "should not fail" $ do
          void $ runStubT'
                    access
                    emptyKube' {
                        images = HashSet.singleton "some-image"
                    } $ do
                          vmId <- prepare'

                          deleteVm vmId

  context "when Pod doesn't exist" $ do
    let vmId = Base.VmId "does-not-exist"
    it "does not fail" $ do
      void $ runStubT'
        access
        emptyKube' {
         images = HashSet.singleton "some-image"
        } $ do
          deleteVm vmId

          maybePod <- getPod "bosh" (Unwrapped vmId)
          lift $ maybePod `shouldSatisfy` isNothing

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message
