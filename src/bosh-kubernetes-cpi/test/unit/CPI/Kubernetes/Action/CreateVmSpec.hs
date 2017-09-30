{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module CPI.Kubernetes.Action.CreateVmSpec(spec) where

import           Test.Hspec

import           Control.Lens

import qualified CPI.Kubernetes.Base64                  as Base64
import CPI.Kubernetes.VmTypes (VmProperties(..), Service(..), emptyVmProperties)

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
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Pod
import qualified CPI.Kubernetes.Resource.Pod            as Pod
import           CPI.Kubernetes.Resource.Secret         (getSecret)
import qualified CPI.Kubernetes.Resource.Secret         as Secret
import           CPI.Kubernetes.Resource.Service        (getService, createService, newService)
import qualified CPI.Kubernetes.Resource.Service        as Service
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.Service
import           CPI.Kubernetes.Resource.Stub.State (KubeState, emptyKube, images, secrets, events, StubConfig, emptyStubConfig, NoOutput)
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
spec = describe "createVm" $ do
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

  it "should create a Pod" $ do
    void $ runStubT'
             access
             emptyKube' {
               images = HashSet.singleton "some-image"
             } $ do
      (Base.VmId vmId) <- createVm
                (Base.AgentId "test-agent")
                (Base.StemcellId "some-image")
                emptyVmProperties
                (Base.Networks HashMap.empty)
                [Base.VolumeId ""]
                (Base.Environment HashMap.empty)

      lift $ vmId `shouldBe` "test-agent"
      maybePod <- getPod "bosh" vmId
      lift $ maybePod `shouldSatisfy` isJust

  describe "should create a Pod" $ do
    it "named after the agent id" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        (Base.VmId vmId) <- createVm
                  (Base.AgentId "test-agent")
                  (Base.StemcellId "some-image")
                  emptyVmProperties
                  (Base.Networks HashMap.empty)
                  [Base.VolumeId ""]
                  (Base.Environment HashMap.empty)

        lift $ vmId `shouldBe` "test-agent"
        maybePod <- getPod "bosh" vmId
        lift $ maybePod `shouldSatisfy` isJust
        let Just pod = maybePod
        lift $ (pod ^. name) `shouldBe` "test-agent"

    it "labeled with agent id" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        (Base.VmId vmId) <- createVm
                  (Base.AgentId "test-agent")
                  (Base.StemcellId "some-image")
                  emptyVmProperties
                  (Base.Networks HashMap.empty)
                  [Base.VolumeId ""]
                  (Base.Environment HashMap.empty)

        lift $ vmId `shouldBe` "test-agent"
        maybePod <- getPod "bosh" vmId
        lift $ (maybePod ^.. _Just.labels.at "bosh.cloudfoundry.org/agent-id"._Just._String) `shouldBe` ["test-agent"]

    it "with the agent settings attached as secret volume" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                         emptyVmProperties
                         (Base.Networks HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybePod <- getPod "bosh" vmId
               lift $ maybePod `shouldSatisfy` isJust
               let Just pod = maybePod
                   volumeNames = pod ^.. (Pod.volumes.each.Volume.name)
                   secretVolumes = pod ^.. (Pod.volumes.each.Volume.secret._Just)
               lift $ volumeNames `shouldContain` ["agent-settings"]
               lift $ secretVolumes `shouldContain` [mkSecretVolumeSource & SecretVolumeSource.secretName .~ Just "agent-settings-test-agent"]

    it "with an empty dir attached" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                         emptyVmProperties
                         (Base.Networks HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybePod <- getPod "bosh" vmId
               lift $ maybePod `shouldSatisfy` isJust
               let Just pod = maybePod
                   volumeNames = pod ^.. (Pod.volumes.each.Volume.name)
               lift $ volumeNames `shouldContain` ["ephemeral-disk"]

    describe "with a container" $ do

      it "using the stemcell id as image id" $ do
        void $ runStubT'
                 access
                 emptyKube' {
                   images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
                 } $ do
                 (Base.VmId vmId) <- createVm
                           (Base.AgentId "test-agent")
                           (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                           emptyVmProperties
                           (Base.Networks HashMap.empty)
                           [Base.VolumeId ""]
                           (Base.Environment HashMap.empty)

                 maybePod <- getPod "bosh" vmId
                 lift $ maybePod `shouldSatisfy` isJust
                 let Just pod = maybePod
                     imageId = pod ^. (Pod.spec._Just.PodSpec.containers.ix 0.Container.image._Just)
                 lift $ imageId `shouldBe` "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"

      it "with ephermeral disk mounted as empty dir volume" $ do
        void $ runStubT'
                 access
                 emptyKube' {
                   images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
                 } $ do
                 (Base.VmId vmId) <- createVm
                           (Base.AgentId "test-agent")
                           (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                           emptyVmProperties
                           (Base.Networks HashMap.empty)
                           [Base.VolumeId ""]
                           (Base.Environment HashMap.empty)

                 lift $ vmId `shouldBe` "test-agent"
                 maybePod <- getPod "bosh" vmId
                 let hasMount :: VolumeMount -> Bool
                     hasMount mount = mount ^. VolumeMount.name == "ephemeral-disk"
                 lift $ (maybePod ^.. _Just.container.Container.volumeMounts._Just.each.VolumeMount.name) `shouldContain` ["ephemeral-disk"]
                 lift $ (maybePod ^.. _Just.container.Container.volumeMounts._Just.each.filtered hasMount.VolumeMount.mountPath) `shouldContain` ["/var/vcap/data"]

      it "with agent settings mounted as secret volume" $ do
        void $ runStubT'
                access
                emptyKube' {
                  images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
                } $ do
                (Base.VmId vmId) <- createVm
                          (Base.AgentId "test-agent")
                          (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                          emptyVmProperties
                          (Base.Networks HashMap.empty)
                          [Base.VolumeId ""]
                          (Base.Environment HashMap.empty)

                lift $ vmId `shouldBe` "test-agent"
                maybePod <- getPod "bosh" vmId
                let hasMount :: VolumeMount -> Bool
                    hasMount mount = mount ^. VolumeMount.name == "agent-settings"
                lift $ (maybePod ^.. _Just.container.Container.volumeMounts._Just.each.VolumeMount.name) `shouldContain` ["agent-settings"]
                lift $ (maybePod ^.. _Just.container.Container.volumeMounts._Just.each.filtered hasMount.VolumeMount.mountPath) `shouldContain` ["/var/vcap/bosh/settings-source-file"]

      it "in priviledged mode" $ do
        void $ runStubT'
                access
                emptyKube' {
                  images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
                } $ do
                (Base.VmId vmId) <- createVm
                          (Base.AgentId "test-agent")
                          (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                          emptyVmProperties
                          (Base.Networks HashMap.empty)
                          [Base.VolumeId ""]
                          (Base.Environment HashMap.empty)

                maybePod <- getPod "bosh" vmId
                lift $ maybePod `shouldSatisfy` isJust
                let Just pod = maybePod
                    priviledged = pod ^.. container.Container.securityContext._Just.SecurityContext.privileged._Just
                lift $ priviledged `shouldBe` [True]
                let userId = pod ^.. container.Container.securityContext._Just.SecurityContext.runAsUser._Just
                lift $ userId `shouldBe` [0]

      it "that launches the agent" $ do
        void $ runStubT'
                access
                emptyKube' {
                  images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
                } $ do
                (Base.VmId vmId) <- createVm
                          (Base.AgentId "test-agent")
                          (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                          emptyVmProperties
                          (Base.Networks HashMap.empty)
                          [Base.VolumeId ""]
                          (Base.Environment HashMap.empty)

                lift $ vmId `shouldBe` "test-agent"
                maybePod <- getPod "bosh" vmId
                lift $ (maybePod ^. _Just.container.Container.command) `shouldBe` Just [
                             "/bin/bash", "-c",
                             "cp /etc/resolv.conf /etc/resolv.conf.dup; "
                          <> "umount /etc/resolv.conf; "
                          <> "mv /etc/resolv.conf.dup /etc/resolv.conf; "
                          <> "cp /etc/hosts /etc/hosts.dup; "
                          <> "umount /etc/hosts; "
                          <> "mv /etc/hosts.dup /etc/hosts; "
                          <> "cp /etc/hostname /etc/hostname.dup; "
                          <> "umount /etc/hostname; "
                          <> "mv /etc/hostname.dup /etc/hostname; "
                          <> "exec env -i /usr/sbin/runsvdir-start"]

      it "with tty" $ do
        void $ runStubT'
                access
                emptyKube' {
                  images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
                } $ do
                (Base.VmId vmId) <- createVm
                          (Base.AgentId "test-agent")
                          (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                          emptyVmProperties
                          (Base.Networks HashMap.empty)
                          [Base.VolumeId ""]
                          (Base.Environment HashMap.empty)

                lift $ vmId `shouldBe` "test-agent"
                maybePod <- getPod "bosh" vmId
                lift $ (maybePod ^? _Just.container.Container.tty._Just) `shouldBe` Just True

      it "with stdin" $ do
        void $ runStubT'
                access
                emptyKube' {
                  images = HashSet.singleton "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest"
                } $ do
                (Base.VmId vmId) <- createVm
                          (Base.AgentId "test-agent")
                          (Base.StemcellId "loewenstein/bosh-stemcell-kubernetes-ubuntu-trusty-go_agent:latest")
                          emptyVmProperties
                          (Base.Networks HashMap.empty)
                          [Base.VolumeId ""]
                          (Base.Environment HashMap.empty)

                lift $ vmId `shouldBe` "test-agent"
                maybePod <- getPod "bosh" vmId
                lift $ (maybePod ^? _Just.container.Container.stdin._Just) `shouldBe` Just True

    it "and wait for the Pod to be running" $ do
     (_, s, _) <- runStubT'
                    access
                    emptyKube' {
                      images = HashSet.singleton "test-stemcell"
                    } $ do
                    (Base.VmId vmId) <- createVm
                              (Base.AgentId "test-agent")
                              (Base.StemcellId "test-stemcell")
                              emptyVmProperties
                              (Base.Networks HashMap.empty)
                              [Base.VolumeId ""]
                              (Base.Environment HashMap.empty)

                    lift $ vmId `shouldBe` "test-agent"
                    maybePod <- getPod "bosh" vmId
                    lift $ maybePod `shouldSatisfy` isJust
                    let Just pod = maybePod
                    lift $ (pod ^. Pod.status.Pod.phase._Just) `shouldBe` "Running"
     (HashMap.size $ events s) `shouldBe` 1
     pure ()

  it "should create a Secret" $ do
    void $ runStubT'
             access
             emptyKube' {
               images = HashSet.singleton "some-image"
             } $ do
             (Base.VmId vmId) <- createVm
                       (Base.AgentId "test-agent")
                       (Base.StemcellId "some-image")
                       emptyVmProperties
                       (Base.Networks HashMap.empty)
                       [Base.VolumeId ""]
                       (Base.Environment HashMap.empty)

             maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
             lift $ maybeSecret `shouldSatisfy` isJust

  describe "should create a Secret" $ do
    it "with name 'agent-settings-<agent id>'" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "some-image")
                         emptyVmProperties
                         (Base.Networks HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
               lift $ (maybeSecret ^. _Just.name) `shouldBe` "agent-settings-test-agent"

    it "labeled with agent id" $ do
      void $ runStubT'
               access
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
               (Base.VmId vmId) <- createVm
                         (Base.AgentId "test-agent")
                         (Base.StemcellId "some-image")
                         emptyVmProperties
                         (Base.Networks HashMap.empty)
                         [Base.VolumeId ""]
                         (Base.Environment HashMap.empty)

               maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
               lift $ (maybeSecret ^.. _Just.labels.at "bosh.cloudfoundry.org/agent-id"._Just._String) `shouldBe` ["test-agent"]

    it "with encoded data" $ do
      let initialSettings = Base.initialAgentSettings (Wrapped "test-agent") (Wrapped HashMap.empty) (Just $ Wrapped blobstore) (Wrapped env) ntp mbus
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
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        (Base.VmId vmId) <- createVm
                  (Base.AgentId "test-agent")
                  (Base.StemcellId "some-image")
                  emptyVmProperties
                  (Base.Networks HashMap.empty)
                  [Base.VolumeId ""]
                  (Base.Environment HashMap.empty)

        maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
        let [encoded] = maybeSecret ^.. _Just.Secret.data'.at "config.json"._Just._String
        decoded <- Base64.decodeJSON encoded
        lift $ decoded `shouldBe` initialSettings

    it "with all networks set to preconfigured" $ do
      --  & HashMap.insert "networks" (toJSON (networkSpec & _Unwrapped.each._Unwrapped._Object.preconfigured .~ Bool True))
      let networks = Wrapped $ HashMap.singleton
                                "default" $
                                Wrapped $ HashMap.fromList [("type", String "dynamic")]
          preconfiguredNetworks :: Base.Networks
          preconfiguredNetworks = Wrapped $ HashMap.singleton
                                "default" $
                                Wrapped $ HashMap.fromList [
                                                              ("type", String "dynamic")
                                                            , ("preconfigured", Bool True)
                                                          ]
          initialSettings = Base.initialAgentSettings (Wrapped "test-agent") preconfiguredNetworks (Just $ Wrapped blobstore) (Wrapped env) ntp mbus
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
               emptyKube' {
                 images = HashSet.singleton "some-image"
               } $ do
        (Base.VmId vmId) <- createVm
                  (Base.AgentId "test-agent")
                  (Base.StemcellId "some-image")
                  emptyVmProperties
                  networks
                  [Base.VolumeId ""]
                  (Base.Environment HashMap.empty)

        maybeSecret <- getSecret "bosh" "agent-settings-test-agent"
        let [encoded] = maybeSecret ^.. _Just.Secret.data'.at "config.json"._Just._String
        decoded <- Base64.decodeJSON encoded
        lift $ decoded `shouldBe` initialSettings

  context "when vm cloud_properties specify Services" $ do
    context "when all Services exists" $ do
      let emptyKube'' =
              emptyKube' {
                  images = HashSet.singleton "some-image"
              }
      it "should point the Services to the Pod" $ do
        void $ runStubT'
                 access
                 emptyKube'' $ do
          service1 <- createService "bosh" $ newService "my-service-1"
          service2 <- createService "bosh" $ newService "my-service-2"
          (Base.VmId vmId) <- createVm
                    (Base.AgentId "test-agent")
                    (Base.StemcellId "some-image")
                    (emptyVmProperties {
                       services = [
                           Service {
                             serviceName = "my-service-1"
                           }
                         , Service {
                             serviceName = "my-service-2"
                           }
                       ]
                    })
                    (Base.Networks HashMap.empty)
                    [Base.VolumeId ""]
                    (Base.Environment HashMap.empty)

          lift $ vmId `shouldBe` "test-agent"
          maybePod <- getPod "bosh" vmId
          lift $ maybePod `shouldSatisfy` isJust
          maybeService <- getService "bosh" "my-service-1"
          lift $ maybeService `shouldSatisfy` isJust
          lift $ maybeService ^.. _Just.label "bosh.cloudfoundry.org/agent-id" `shouldBe` ["test-agent"]
          lift $ maybeService ^.. _Just.Service.podSelector.at "bosh.cloudfoundry.org/agent-id"._Just `shouldBe` ["test-agent"]

          maybeService <- getService "bosh" "my-service-2"
          lift $ maybeService `shouldSatisfy` isJust
          lift $ maybeService ^.. _Just.label "bosh.cloudfoundry.org/agent-id" `shouldBe` ["test-agent"]
          lift $ maybeService ^.. _Just.Service.podSelector.at "bosh.cloudfoundry.org/agent-id"._Just `shouldBe` ["test-agent"]

    context "when Services don't exist" $ do
      it "should fail" $ do
        let createVmAction = void $ runStubT'
                               access
                               emptyKube' {
                                   images = HashSet.singleton "some-image"
                               } $ do
                                    void $ createVm
                                              (Base.AgentId "test-agent")
                                              (Base.StemcellId "some-image")
                                              (emptyVmProperties {
                                                 services = [
                                                   Service {
                                                     serviceName = "my-service"
                                                   }
                                                 ]
                                              })
                                              (Base.Networks HashMap.empty)
                                              [Base.VolumeId ""]
                                              (Base.Environment HashMap.empty)
        createVmAction `shouldThrow` cloudErrorWithMessage "Service 'my-service' could not be found."

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message
