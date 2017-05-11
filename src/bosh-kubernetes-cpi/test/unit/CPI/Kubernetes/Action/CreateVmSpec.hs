{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module CPI.Kubernetes.Action.CreateVmSpec(spec) where

import           Test.Hspec

import           Control.Lens
import           Data.Aeson
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Text                           (Text)

import           Kubernetes.Model.V1.Container       (Container)
import qualified Kubernetes.Model.V1.Container       as Container
import           Kubernetes.Model.V1.Pod             (Pod)
import qualified Kubernetes.Model.V1.Pod             as Pod
import           Kubernetes.Model.V1.PodSpec         (PodSpec)
import qualified Kubernetes.Model.V1.PodSpec         as PodSpec
import           Kubernetes.Model.V1.SecurityContext (SecurityContext)
import qualified Kubernetes.Model.V1.SecurityContext as SecurityContext

import           Control.Exception.Safe
import           Control.Monad.Reader
import           Data.Maybe

import qualified CPI.Base                            as Base

import           Control.Monad.Stub.Console
import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.StubMonad

import           Control.Monad.Stub.Wait
import           CPI.Kubernetes.Action.CreateVm
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Pod
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.State  hiding (NoOutput)
import           Resource

instance HasStdin Config where
  asStdin = const ""

data NoOutput = NoOutput

instance Monoid NoOutput where
  mempty = NoOutput
  mappend _ _ = NoOutput

instance HasStdout NoOutput where
  asStdout = mempty
instance HasStderr NoOutput where
  asStderr = mempty
instance HasWaitCount NoOutput where
  asWaitCount = mempty

spec :: Spec
spec = describe "createVm" $ do
  it "should create a Pod named after the agent id" $ do
    let access = Config {
      clusterAccess = ClusterAccess {
        namespace = pure "bosh"
      }
    }
    void $ (runStubT :: Config -> KubeState -> StubT Config KubeState NoOutput IO () -> IO ((), KubeState, NoOutput)) access emptyKube $ do
      (Base.VmId vmId) <- createVm
                (Base.AgentId "test-agent")
                (Base.StemcellId "")
                (Base.VmProperties $ Object HashMap.empty)
                (Base.Networks $ Object HashMap.empty)
                [Base.VolumeId ""]
                (Base.Environment HashMap.empty)

      lift $ vmId `shouldBe` "test-agent"
      maybePod <- getPod "bosh" vmId
      lift $ maybePod `shouldSatisfy` isJust
      let Just pod = maybePod
      lift $ (pod ^. name) `shouldBe` "test-agent"

  it "should create a Pod using the stemcell id as image id" $ do
    let access = Config {
      clusterAccess = ClusterAccess {
        namespace = pure "bosh"
      }
    }
    void $ (runStubT :: Config -> KubeState -> StubT Config KubeState NoOutput IO () -> IO ((), KubeState, NoOutput)) access emptyKube $ do
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
    let access = Config {
      clusterAccess = ClusterAccess {
        namespace = pure "bosh"
      }
    }
    void $ (runStubT :: Config -> KubeState -> StubT Config KubeState NoOutput IO () -> IO ((), KubeState, NoOutput)) access emptyKube $ do
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

  -- it "should wait for the pod to be running" $ do
  --   return expression
