{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module ServiceSpec(spec) where

import           Prelude                              hiding (readFile)

import           StubRunner
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
import           CPI.Kubernetes.Resource.Service      (Services,
                                                       createService,
                                                       deleteService,
                                                       getService, listService,
                                                       newService,
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
import Control.Monad.Reader
import           Control.Effect.Class.FileSystem

import           Data.Typeable
import           GHC.Stack.Types
import           Network.HTTP.Types.Status
import           Servant.Common.Req                   (ServantError (..))
import           System.Environment


withService :: (Services m, MonadThrow m, MonadMask m, HasConfig c, MonadReader c m) => Service -> m a -> m a
withService service action =
  bracket
    (do
      config <- asks asConfig
      let ns = namespace $ clusterAccess config
      createdService <- createService ns service
      pure (service, createdService))
    (\_ -> do
      config <- asks asConfig
      let ns = namespace $ clusterAccess config
      deleteService ns $ service ^. name)
    (\(service, createdService) -> do
      action
      )


spec :: Spec
spec = describe "Services" $ do
  let newTestService name = newService name
                  & Service.spec ?~ serviceSpec
      serviceSpec = newServiceSpec [servicePort]
      servicePort = newServicePort "test-port" 80
      testService = newTestService "test-service"
  describe "getService" $ do
    it "returns the service" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withService testService $ do
          service <- getService ns $ testService ^. name
          lift $ service `shouldSatisfy` isJust
          lift $ (service ^. _Just.name) `shouldBe` (testService ^. name)

  describe "listService" $ do
    it "returns the list of services" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withService testService $ do
          services <- listService ns Nothing
          lift $ services ^.. Service.services.each.name `shouldBe` ["test-service"]
    context "when a selector is given" $ do
      let testServiceWithLabel = newTestService "test-service-with-label" & label "select" .~ "match"
      it "returns the list of services matching the selector" $ do
        void $ run emptyStubConfig emptyKube $ do
          config <- asks asConfig
          let ns = namespace $ clusterAccess config
          withService testService $ do
            withService testServiceWithLabel $ do
              services <- listService ns $ Just "select=match"
              lift $ services ^.. Service.services.each.name `shouldBe` ["test-service-with-label"]

  describe "update" $ do
    it "can add a label to the service" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withService testService $ do
          maybeService <- getService ns $ testService ^. name
          lift $ maybeService `shouldSatisfy` isJust
          let service = fromJust maybeService
          let assignedService = service
                              & label "bosh.cloudfoundry.org/agent-id" .~ "test-agent"
          updateService ns assignedService

          updatedService <- getService ns $ testService ^. name
          lift $ updatedService ^.. _Just.label "bosh.cloudfoundry.org/agent-id"
                            `shouldBe` ["test-agent"]

    it "can add a selector to the service" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withService testService $ do
          config <- asks asConfig
          let ns = namespace $ clusterAccess config
          maybeService <- getService ns $ testService ^. name
          lift $ maybeService `shouldSatisfy` isJust
          let service = fromJust maybeService
          let assignedService = service
                              & podSelector.at "bosh.cloudfoundry.org/agent-id" ?~ String "test-agent"
          updateService ns assignedService

          updatedService <- getService ns $ testService ^. name
          lift $ updatedService ^.. _Just.podSelector.at "bosh.cloudfoundry.org/agent-id"._Just
                            `shouldBe` ["test-agent"]


servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message
