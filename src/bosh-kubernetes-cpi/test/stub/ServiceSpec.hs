{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module ServiceSpec(spec) where

import           Data.Monoid
import           Debug.Trace

import           Prelude                              hiding (readFile)

import           Test.Hspec

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Lens

import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe

import qualified CPI.Base                             as Base
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Service      (MonadService,
                                                       createService,
                                                       deleteService,
                                                       getService, newService,
                                                       newServicePort,
                                                       newServiceSpec,
                                                       podSelector,
                                                       updateService,
                                                       waitForService)
import qualified CPI.Kubernetes.Resource.Service      as Service
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Service
import           CPI.Kubernetes.Resource.Stub.State

import           Kubernetes.Model.V1.Container        (Container, mkContainer)
import qualified Kubernetes.Model.V1.Container        as Container
import           Kubernetes.Model.V1.ObjectMeta       (ObjectMeta, mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta       as ObjectMeta
import           Kubernetes.Model.V1.Pod              (Pod, mkPod)
import qualified Kubernetes.Model.V1.Pod              as Pod
import           Kubernetes.Model.V1.PodList          (PodList, mkPodList)
import qualified Kubernetes.Model.V1.PodList          as PodList
import           Kubernetes.Model.V1.PodSpec          (PodSpec, mkPodSpec)
import qualified Kubernetes.Model.V1.PodSpec          as PodSpec
import           Kubernetes.Model.V1.PodStatus        (PodStatus, mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus        as PodStatus
import           Kubernetes.Model.V1.Service          (Service, mkService)
import qualified Kubernetes.Model.V1.Service          as Service
import           Kubernetes.Model.V1.Volume           (Volume, mkVolume)
import qualified Kubernetes.Model.V1.Volume           as Volume
import           Kubernetes.Model.V1.VolumeMount      (VolumeMount,
                                                       mkVolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount      as VolumeMount

import           Network.TLS
import           Resource
import           Servant.Common.BaseUrl               (BaseUrl, parseBaseUrl)


import qualified Data.HashSet                         as HashSet
import           Data.Text                            (Text)
import qualified Data.Text                            as Text

import           Control.Exception.Safe
import           Control.Monad.FileSystem
import           Control.Monad.Stub.StubMonad
import           Data.Typeable
import           GHC.Stack.Types
import           Network.HTTP.Types.Status
import           Servant.Common.Req                   (ServantError (..))
import           System.Environment


withService :: (MonadService m, MonadThrow m, MonadMask m) => Text -> Service -> m a -> m a
withService namespace service action =
  bracket
    (do
      createdService <- createService "default" service
      pure (service, createdService))
    (\_ -> do
      deleteService "default" $ service ^. name)
    (\(service, createdService) -> do
      action
      )


spec :: Spec
spec = describe "MonadService" $ do
  let testService = newService "test-service"
                  & Service.spec ?~ serviceSpec
      serviceSpec = newServiceSpec [servicePort]
      servicePort = newServicePort "test-port" 80
  describe "getService" $ do
    it "returns the service" $ do
      void $ run $ do
        withService "default" testService $ do
          service <- getService "default" $ testService ^. name
          lift $ service `shouldSatisfy` isJust
          lift $ (service ^. _Just.name) `shouldBe` (testService ^. name)

  describe "update" $ do
    it "can add a label to the service" $ do
      void $ run $ do
        withService "default" testService $ do
          maybeService <- getService "default" $ testService ^. name
          lift $ maybeService `shouldSatisfy` isJust
          let service = fromJust maybeService
          let assignedService = service
                              & label "bosh.cloudfoundry.org/agent-id" .~ "test-agent"
          updateService "default" assignedService

          updatedService <- getService "default" $ testService ^. name
          lift $ updatedService ^.. _Just.label "bosh.cloudfoundry.org/agent-id"
                            `shouldBe` ["test-agent"]

    it "can add a selector to the service" $ do
      void $ run $ do
        withService "default" testService $ do
          maybeService <- getService "default" $ testService ^. name
          lift $ maybeService `shouldSatisfy` isJust
          let service = fromJust maybeService
          let assignedService = service
                              & podSelector.at "bosh.cloudfoundry.org/agent-id" ?~ String "test-agent"
          updateService "default" assignedService

          updatedService <- getService "default" $ testService ^. name
          lift $ updatedService ^.. _Just.podSelector.at "bosh.cloudfoundry.org/agent-id"._Just
                            `shouldBe` ["test-agent"]


servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message

type MR a = (forall m . (MonadService (m IO), MonadTrans m, MonadMask (m IO), MonadThrow (m IO)) => (m IO) a)

run :: MR a -> IO a
run f = do
  cluster <- read . fromMaybe "False" <$> lookupEnv "KUBE_CLUSTER"
  if cluster then
          config `runResource` f
        else do
          (result, _, _::NoOutput) <- runStubT
                                        emptyStubConfig
                                        emptyKube
                                        f
          pure result

config :: Config
config = Config {
    clusterAccess = access
  , agent = undefined
}

access :: ClusterAccess
access = ClusterAccess {
    server = parseBaseUrl "https://192.168.99.100:8443"
  , namespace = pure "default"
  , credentials = ClientCertificate <$> readCredential "/Users/d043856/.minikube/apiserver.crt" "/Users/d043856/.minikube/apiserver.key"
}


readCredential :: (MonadThrow m, MonadFileSystem m) => Text -> Text -> m Credential
readCredential certPath keyPath = do
  cert <- readFile certPath
  key <- readFile keyPath
  either
    (throwM . Base.ConfigParseException)
    pure
    (credentialLoadX509FromMemory cert key)
