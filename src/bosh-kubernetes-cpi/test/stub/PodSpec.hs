{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module PodSpec(spec) where

import           Prelude                                hiding (readFile)

import           Test.Hspec

import           Control.Lens

import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe

import           CPI.Base                               as Base
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Pod            (MonadPod, createPod,
                                                         deletePod, getPod,
                                                         newContainer, newPod,
                                                         waitForPod)
import qualified CPI.Kubernetes.Resource.Pod            as Pod
import           CPI.Kubernetes.Resource.Secret
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.State
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
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
    let pod = newPod "test" container
        container = newContainer "busybox" "busybox"
    it "creates a pod with the given name" $ do
      void $ run $ do
        bracket
          (do
            createdPod <- createPod "default" pod
            pure (pod, createdPod))
          (\_ -> do
            deletePod "default" "test")
          (\(pod, createdPod) -> do
            let createdPodName = createdPod ^. Pod.metadata._Just.ObjectMeta.name._Just
            lift $ createdPodName `shouldBe` "test"
            pod' <- getPod "default" "test"
            lift $ pod' `shouldSatisfy` isJust
            lift $ (pod ^. Pod.name) `shouldBe` (createdPod ^. Pod.name)
            )

    it "creates a pod with default service account" $ do
      void $ run $ do
        bracket
          (do
            createdPod <- createPod "default" pod
            pure (pod, createdPod))
          (\_ -> do
            deletePod "default" "test")
          (\(pod, createdPod) -> do
            lift $ (createdPod ^. Pod.name) `shouldBe` "test"
            lift $ createdPod `shouldSatisfy` hasServiceAccount "default"
            lift $ createdPod `shouldSatisfy` hasSecretVolume "default-token"
            lift $ (createdPod ^.. Pod.container.Container.volumeMounts._Just.each.VolumeMount.mountPath) `shouldBe` ["/var/run/secrets/kubernetes.io/serviceaccount"]
            lift $ (createdPod ^.. Pod.container.Container.volumeMounts._Just.each.VolumeMount.readOnly) `shouldBe` [Just True]
            )

    it "creates a pod in state 'Pending'" $ do
      void $ run $ do
        bracket
          (do
            createdPod <- createPod "default" pod
            pure (pod, createdPod))
          (\_ -> do
            deletePod "default" "test")
          (\(pod, createdPod) -> do
            lift $ (createdPod ^. Pod.status._Just.PodStatus.phase) `shouldBe` Just "Pending"
            )

    context "when a pod with the given name already exists" $
      it "throws ServantError reason 409 CONFLICT" $ do
        void $ run $
          bracket
            (createPod "default" pod)
            (\_ -> deletePod "default" "test")
            (\_ -> createPod "default" pod)
        `shouldThrow` (servantErrorWithStatusCode 409)

    context "when image does not exist" $ do
      let pod = newPod "test" container
          container = newContainer "busybox" "does-not-exist"
      it "should time out waiting for state RUNNING" $ do
        void $ run $
          bracket
            (createPod "default" pod)
            (\_ -> deletePod "default" "test")
            (\_ -> waitForPod "default" "test" (const False))
        `shouldThrow` (cloudErrorWithMessage "Timeout waiting for pod")

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector CloudError
cloudErrorWithMessage expectedMessage (CloudError message) = expectedMessage == message

hasSecretVolume name pod = let
  volumes = pod ^. Pod.spec._Just.PodSpec.volumes._Just
  secretVolumeName v = v ^. Volume.secret._Just.SecretVolumeSource.secretName._Just
  in
    any (\v -> name `Text.isPrefixOf` secretVolumeName v) volumes

hasServiceAccount name pod = let
  serviceAccountName = pod ^. Pod.spec._Just.PodSpec.serviceAccountName._Just
  in
    name == serviceAccountName

type MR a = (forall m . (MonadPod (m IO), MonadSecret (m IO), MonadTrans m, MonadMask (m IO), MonadThrow (m IO)) => (m IO) a)

run :: MR a -> IO a
run f = do
  cluster <- read . fromMaybe "False" <$> lookupEnv "KUBE_CLUSTER"
  if cluster then
          config `runResource` f
        else do
          (result, _, _::()) <- runStubT undefined emptyKube f
          pure result
          -- runResourceStub undefined emptyKube f

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
