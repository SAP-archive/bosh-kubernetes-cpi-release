{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
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
import           CPI.Kubernetes.Resource.Secret         (MonadSecret,
                                                         createSecret,
                                                         deleteSecret,
                                                         getSecret, newSecret,
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

import           Control.Exception.Safe
import           Control.Monad.FileSystem
import           Control.Monad.Reader
import           Control.Monad.Stub.StubMonad
import           Data.Typeable
import           GHC.Stack.Types
import           Network.HTTP.Types.Status
import           Servant.Common.Req                     (ServantError (..))
import           System.Environment

spec :: Spec
spec =
  describe "create" $ do
    let secret = newSecret "test"
    it "creates a secret with the given name" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        bracket
          (do
            createdSecret <- createSecret ns secret
            pure (secret, createdSecret))
          (\_ -> do
            deleteSecret ns "test")
          (\(secret, createdSecret) -> do
            let createdSecretName = createdSecret ^. Secret.metadata._Just.ObjectMeta.name._Just
            lift $ createdSecretName `shouldBe` "test"
            secret' <- getSecret ns "test"
            lift $ secret' `shouldSatisfy` isJust
            lift $ (secret ^. name) `shouldBe` (createdSecret ^. name)
            )

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message
