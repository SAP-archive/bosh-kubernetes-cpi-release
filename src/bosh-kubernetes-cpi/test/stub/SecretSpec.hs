{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module SecretSpec(spec) where

import           Prelude                                hiding (readFile)

import           Test.Hspec

import           Control.Lens

import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe

import           CPI.Base                               as Base
import           CPI.Kubernetes.Config
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
      void $ run $ do
        bracket
          (do
            createdSecret <- createSecret "default" secret
            pure (secret, createdSecret))
          (\_ -> do
            deleteSecret "default" "test")
          (\(secret, createdSecret) -> do
            let createdSecretName = createdSecret ^. Secret.metadata._Just.ObjectMeta.name._Just
            lift $ createdSecretName `shouldBe` "test"
            secret' <- getSecret "default" "test"
            lift $ secret' `shouldSatisfy` isJust
            lift $ (secret ^. Secret.name) `shouldBe` (createdSecret ^. Secret.name)
            )

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector CloudError
cloudErrorWithMessage expectedMessage (CloudError message) = expectedMessage == message

type MR a = (forall m . (MonadSecret (m IO), MonadTrans m, MonadMask (m IO), MonadThrow (m IO)) => (m IO) a)

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
