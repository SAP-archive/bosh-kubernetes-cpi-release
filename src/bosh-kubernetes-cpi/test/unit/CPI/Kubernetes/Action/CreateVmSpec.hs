{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module CPI.Kubernetes.Action.CreateVmSpec(spec) where

import           Test.Hspec

import           Control.Lens
import qualified CPI.Kubernetes.Base64               as Base64
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy                (fromStrict, toStrict)
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.HashSet                        (HashSet)
import qualified Data.HashSet                        as HashSet
import           Data.Text                           (Text)
import           Data.Text.Encoding

import           Kubernetes.Model.V1.Container       (Container)
import qualified Kubernetes.Model.V1.Container       as Container
import           Kubernetes.Model.V1.Pod             (Pod)
import qualified Kubernetes.Model.V1.Pod             as Pod
import           Kubernetes.Model.V1.PodSpec         (PodSpec)
import qualified Kubernetes.Model.V1.PodSpec         as PodSpec
import           Kubernetes.Model.V1.PodStatus       (PodStatus)
import qualified Kubernetes.Model.V1.PodStatus       as PodStatus
import           Kubernetes.Model.V1.SecurityContext (SecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext as SecurityContext

import           Control.Exception.Safe
import           Control.Monad.Reader

import           Data.Maybe
import           Data.Semigroup

import qualified CPI.Base                            as Base

import           Control.Monad.Stub.Console
import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.StubMonad

import           Control.Monad.Stub.Wait
import           CPI.Kubernetes.Action.CreateVm
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Pod
import           CPI.Kubernetes.Resource.Secret      (getSecret)
import qualified CPI.Kubernetes.Resource.Secret      as Secret
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.State
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
spec = describe "createVm" $ do
  let access :: (?agent :: Object) => TestConfig
      access = TestConfig {
                 config = Config {
                   clusterAccess = ClusterAccess {
                     namespace = pure "bosh"
                   }
                   , agent = ?agent
                 }
                 , stubConfig = emptyStubConfig
               }
  let ?agent = HashMap.empty :: HashMap Text Value
  it "should create a Pod" $ do
    void $ runStubT'
             access
             emptyKube {
               images = HashSet.singleton "some-image"
             } $ do
      (Base.VmId vmId) <- createVm
                (Base.AgentId "test-agent")
                (Base.StemcellId "some-image")
                (Base.VmProperties $ Object HashMap.empty)
                (Base.Networks $ Object HashMap.empty)
                [Base.VolumeId ""]
                (Base.Environment HashMap.empty)

      lift $ vmId `shouldBe` "test-agent"
      maybePod <- getPod "bosh" vmId
      lift $ maybePod `shouldSatisfy` isJust

  describe "should create a Pod" $ do
    it "should create a Pod named after the agent id" $ do
      void $ runStubT'
               access
               emptyKube {
                 images = HashSet.singleton "some-image"
               } $ do
        (Base.VmId vmId) <- createVm
                  (Base.AgentId "test-agent")
                  (Base.StemcellId "some-image")
                  (Base.VmProperties $ Object HashMap.empty)
                  (Base.Networks $ Object HashMap.empty)
                  [Base.VolumeId ""]
                  (Base.Environment HashMap.empty)

        lift $ vmId `shouldBe` "test-agent"
        maybePod <- getPod "bosh" vmId
        lift $ maybePod `shouldSatisfy` isJust
        let Just pod = maybePod
        lift $ (pod ^. name) `shouldBe` "test-agent"

    it "should create a Pod labeled with agent id" $ do
      void $ runStubT'
               access
               emptyKube {
                 images = HashSet.singleton "some-image"
               } $ do
        (Base.VmId vmId) <- createVm
                  (Base.AgentId "test-agent")
                  (Base.StemcellId "some-image")
                  (Base.VmProperties $ Object HashMap.empty)
                  (Base.Networks $ Object HashMap.empty)
                  [Base.VolumeId ""]
                  (Base.Environment HashMap.empty)

        lift $ vmId `shouldBe` "test-agent"
        maybePod <- getPod "bosh" vmId
        lift $ (maybePod ^.. _Just.labels.at "bosh.cloudfoundry.org/agent-id"._Just._String) `shouldBe` ["test-agent"]

    it "should create a Pod using the stemcell id as image id" $ do
      void $ runStubT'
               access
               emptyKube {
                 images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                         (Base.VmProperties $ Object HashMap.empty)
                         (Base.Networks $ Object HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybePod <- getPod "bosh" vmId
               lift $ maybePod `shouldSatisfy` isJust
               let Just pod = maybePod
                   imageId = pod ^. (Pod.spec._Just.PodSpec.containers.ix 0.Container.image._Just)
               lift $ imageId `shouldBe` "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
    it "should create a Pod with priviledged container" $ do
     void $ runStubT'
              access
              emptyKube {
                images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
              } $ do
              (Base.VmId vmId) <- createVm
                        (Base.AgentId "test-agent")
                        (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                        (Base.VmProperties $ Object HashMap.empty)
                        (Base.Networks $ Object HashMap.empty)
                        [Base.VolumeId ""]
                        (Base.Environment HashMap.empty)

              maybePod <- getPod "bosh" vmId
              lift $ maybePod `shouldSatisfy` isJust
              let Just pod = maybePod
                  priviledged = pod ^.. container.Container.securityContext._Just.SecurityContext.privileged._Just
              lift $ priviledged `shouldBe` [True]
              let userId = pod ^.. container.Container.securityContext._Just.SecurityContext.runAsUser._Just
              lift $ userId `shouldBe` [0]

    it "should wait for the Pod to be running" $ do
     (_, s, _) <- runStubT'
                    access
                    emptyKube {
                      images = HashSet.singleton "test-stemcell"
                    } $ do
                    (Base.VmId vmId) <- createVm
                              (Base.AgentId "test-agent")
                              (Base.StemcellId "test-stemcell")
                              (Base.VmProperties $ Object HashMap.empty)
                              (Base.Networks $ Object HashMap.empty)
                              [Base.VolumeId ""]
                              (Base.Environment HashMap.empty)

                    lift $ vmId `shouldBe` "test-agent"
                    maybePod <- getPod "bosh" vmId
                    lift $ maybePod `shouldSatisfy` isJust
                    let Just pod = maybePod
                    lift $ (pod ^. Pod.status._Just.PodStatus.phase) `shouldBe` Just "Running"
     (HashMap.size $ events s) `shouldBe` 1
     pure ()

  it "should create a Secret" $ do
    void $ runStubT'
             access
             emptyKube {
               images = HashSet.singleton "some-image"
             } $ do
             (Base.VmId vmId) <- createVm
                       (Base.AgentId "test-agent")
                       (Base.StemcellId "some-image")
                       (Base.VmProperties $ Object HashMap.empty)
                       (Base.Networks $ Object HashMap.empty)
                       [Base.VolumeId ""]
                       (Base.Environment HashMap.empty)

             maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
             lift $ maybeSecret `shouldSatisfy` isJust

  describe "should create a Secret" $ do
    it "with name 'agent-settings-<agent id>'" $ do
      void $ runStubT'
               access
               emptyKube {
                 images = HashSet.singleton "some-image"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "some-image")
                         (Base.VmProperties $ Object HashMap.empty)
                         (Base.Networks $ Object HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
               lift $ (maybeSecret ^. _Just.name) `shouldBe` "agent-settings-test-agent"

    it "labeled with agent id" $ do
      void $ runStubT'
               access
               emptyKube {
                 images = HashSet.singleton "some-image"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "some-image")
                         (Base.VmProperties $ Object HashMap.empty)
                         (Base.Networks $ Object HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
               lift $ (maybeSecret ^.. _Just.labels.at "bosh.cloudfoundry.org/agent-id"._Just._String) `shouldBe` ["test-agent"]

    it "with encoded data" $ do
      let initialSettings = Base.initialAgentSettings (Wrapped "test-agent") (Just $ Wrapped blobstore) (Wrapped env) ntp mbus
          blobstore = HashMap.singleton "provider" "local"
          env = HashMap.empty
          ntp = ["my.time.host"]
          mbus = "http://my.mbus:1234"
      let ?agent = HashMap.fromList [
                  ("mbus" :: Text, toJSON mbus)
                , ("blobstore", toJSON blobstore)
                , ("ntp", toJSON ntp)]

      void $ runStubT'
               access
               emptyKube {
                 images = HashSet.singleton "some-image"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "some-image")
                         (Base.VmProperties $ Object HashMap.empty)
                         (Base.Networks $ Object HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
               let [encoded] = maybeSecret ^.. _Just.Secret.data'.at "settings.json"._Just._String
               decoded <- Base64.decodeJSON encoded
               lift $ decoded `shouldBe` initialSettings



