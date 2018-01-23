{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module SecretSpec(spec) where

import           Prelude                                hiding (readFile)

import           StubRunner
import           Test.Hspec

import           Control.Lens

import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe

import qualified CPI.Base                               as Base
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Secret         (Secrets, createSecret,
                                                         deleteSecret,
                                                         getSecret, listSecret,
                                                         newSecret,
                                                         waitForSecret)
import qualified CPI.Kubernetes.Resource.Secret         as Secret
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.State
import           Kubernetes.Model.V1.Container          (Container, mkContainer)
import qualified Kubernetes.Model.V1.Container          as Container
import           Kubernetes.Model.V1.ObjectMeta         (ObjectMeta,
                                                         mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta         as ObjectMeta
import           Kubernetes.Model.V1.Pod                (Pod, mkPod)
import qualified Kubernetes.Model.V1.Pod                as Pod
import           Kubernetes.Model.V1.PodList            (PodList, mkPodList)
import qualified Kubernetes.Model.V1.PodList            as PodList
import           Kubernetes.Model.V1.PodSpec            (PodSpec, mkPodSpec)
import qualified Kubernetes.Model.V1.PodSpec            as PodSpec
import           Kubernetes.Model.V1.PodStatus          (PodStatus, mkPodStatus)
import qualified Kubernetes.Model.V1.PodStatus          as PodStatus
import qualified Kubernetes.Model.V1.Secret             as Secret
import           Kubernetes.Model.V1.SecretVolumeSource (SecretVolumeSource,
                                                         mkSecretVolumeSource)
import qualified Kubernetes.Model.V1.SecretVolumeSource as SecretVolumeSource
import           Kubernetes.Model.V1.Volume             (Volume, mkVolume)
import qualified Kubernetes.Model.V1.Volume             as Volume
import           Kubernetes.Model.V1.VolumeMount        (VolumeMount,
                                                         mkVolumeMount)
import qualified Kubernetes.Model.V1.VolumeMount        as VolumeMount
import           Network.TLS
import           Resource
import           Servant.Common.BaseUrl                 (BaseUrl, parseBaseUrl)


import qualified Data.HashSet                           as HashSet
import           Data.Text                              (Text)
import qualified Data.Text                              as Text

import           Control.Effect.Class.FileSystem
import           Control.Exception.Safe
import           Control.Monad.Reader

import           Data.Typeable
import           GHC.Stack.Types
import           Network.HTTP.Types.Status
import           Servant.Common.Req                     (ServantError (..))
import           System.Environment


withSecret :: (Secrets m, MonadThrow m, MonadMask m, HasConfig c, MonadReader c m) => Secret -> m a -> m a
withSecret secret action =
  bracket
    (do
      config <- asks asConfig
      let ns = namespace $ clusterAccess config
      createdSecret <- createSecret ns secret
      pure (secret, createdSecret))
    (\_ -> do
      config <- asks asConfig
      let ns = namespace $ clusterAccess config
      deleteSecret ns $ secret ^. name)
    (\(secret, createdSecret) -> do
      action
      )

spec :: Spec
spec = describe "Secret" $ do
  let newTestSecret name = newSecret name
      testSecret = newTestSecret "test-secret"
  describe "createSecret" $ do
    it "creates a secret with the given name" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        bracket
          (do
            createdSecret <- createSecret ns $ testSecret
            pure (testSecret, createdSecret))
          (\_ -> do
            deleteSecret ns "test-secret")
          (\(secret, createdSecret) -> do
            let createdSecretName = createdSecret ^. Secret.metadata._Just.ObjectMeta.name._Just
            lift $ createdSecretName `shouldBe` "test-secret"
            secret' <- getSecret ns "test-secret"
            lift $ secret' `shouldSatisfy` isJust
            lift $ (secret' ^. _Just.name) `shouldBe` (createdSecret ^. name)
            )
  describe "listSecret" $ do
    it "returns the list of secrets" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withSecret testSecret $ do
          secrets <- listSecret ns Nothing
          lift $ secrets ^.. Secret.secrets.each.name `shouldBe` ["test-secret"]
    context "when a selector is given" $ do
      let testSecretWithLabel = newTestSecret "test-secret-with-label" & label "select" .~ "match"
      it "returns the list of secrets matching the selector" $ do
        void $ run emptyStubConfig emptyKube $ do
          config <- asks asConfig
          let ns = namespace $ clusterAccess config
          withSecret testSecret $ do
            withSecret testSecretWithLabel $ do
              secrets <- listSecret ns $ Just "select=match"
              lift $ secrets ^.. Secret.secrets.each.name `shouldBe` ["test-secret-with-label"]

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message
