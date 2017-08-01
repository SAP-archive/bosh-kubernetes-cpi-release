{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module PodSpec(spec) where

import           Debug.Trace

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
import qualified CPI.Kubernetes.Resource.Metadata       as Metadata
import           CPI.Kubernetes.Resource.Pod            (MonadPod, createPod,
                                                         deletePod, getPod,
                                                         newContainer, newPod,
                                                         waitForPod)
import qualified CPI.Kubernetes.Resource.Pod            as Pod
import           CPI.Kubernetes.Resource.Secret
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.State
import           Kubernetes.Model.V1.Container          (Container, mkContainer)
import qualified Kubernetes.Model.V1.Container          as Container
import           Kubernetes.Model.V1.ObjectMeta         (ObjectMeta,
                                                         mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta         as ObjectMeta
import           Kubernetes.Model.V1.Pod                (Pod, mkPod)
import qualified Kubernetes.Model.V1.Pod                as Pod hiding (status)
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


import qualified Data.HashMap.Strict                    as HashMap
import qualified Data.HashSet                           as HashSet
import           Data.Text                              (Text)
import qualified Data.Text                              as Text

import           Control.Exception.Safe
import           Control.Monad.FileSystem
import           Control.Monad.Stub.StubMonad
import           Control.Monad.Wait
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
      void $ run emptyStubConfig emptyKube $ do
        bracket
          (do
            createdPod <- createPod "default" pod
            pure (pod, createdPod))
          (\_ -> do
            deletePod "default" "test"
            waitForPod "default" "test" isNothing)
          (\(pod, createdPod) -> do
            let createdPodName = createdPod ^. Pod.metadata._Just.ObjectMeta.name._Just
            lift $ createdPodName `shouldBe` "test"
            pod' <- getPod "default" "test"
            lift $ pod' `shouldSatisfy` isJust
            lift $ (pod ^. name) `shouldBe` (createdPod ^. name)
            )

    it "creates a pod with default service account" $ do
      void $ run emptyStubConfig emptyKube $ do
        bracket
          (do
            createdPod <- createPod "default" pod
            pure (pod, createdPod))
          (\_ -> do
            deletePod "default" "test"
            waitForPod "default" "test" isNothing)
          (\(pod, createdPod) -> do
            lift $ (createdPod ^. name) `shouldBe` "test"
            lift $ createdPod `shouldSatisfy` hasServiceAccount "default"
            lift $ createdPod `shouldSatisfy` hasSecretVolume "default-token"
            lift $ (createdPod ^.. Pod.container.Container.volumeMounts._Just.each.VolumeMount.mountPath) `shouldBe` ["/var/run/secrets/kubernetes.io/serviceaccount"]
            lift $ (createdPod ^.. Pod.container.Container.volumeMounts._Just.each.VolumeMount.readOnly) `shouldBe` [Just True]
            )

    it "creates a pod in state 'Pending'" $ do
      void $ run emptyStubConfig emptyKube $ do
        bracket
          (do
            createdPod <- createPod "default" pod
            pure (pod, createdPod))
          (\_ -> do
            deletePod "default" "test"
            waitForPod "default" "test" isNothing)
          (\(pod, createdPod) -> do
            lift $ (createdPod ^. Pod.status.Pod.phase._Just) `shouldBe` "Pending"
            )

    it "creates a pod that will end up in state 'Running'" $ do
      void $ run
              emptyStubConfig
              emptyKube {
                  images = HashSet.singleton "busybox"
                , secrets = HashMap.singleton ("default", "default-token") (newSecret "default-token")
              } $ do
        bracket
          (do
            _ <- createPod "default" pod
            pure pod)
          (\_ -> do
            deletePod "default" "test"
            waitForPod "default" "test" isNothing)
          (\pod -> do
            runningPod <- waitForPod "default" "test" (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
            lift $ (runningPod ^. _Just.Pod.status.Pod.phase._Just) `shouldBe` "Running"
            )

    context "when Pod has a secret volume attached" $ do
      let pod' = pod & Pod.volumes %~ (settingsVolume <|)
          settingsVolume =  mkVolume "secret-volume"
                            & Volume.secret .~ (Just $ mkSecretVolumeSource
                            & SecretVolumeSource.secretName .~ (Just $ secret ^. Metadata.name))
          secret = newSecret "test-secret"
      it "creates a pod that will end up in state 'Running'" $ do
        void $ run
                emptyStubConfig
                emptyKube {
                    images = HashSet.singleton "busybox"
                  , secrets = HashMap.singleton ("default", "default-token") (newSecret "default-token")
                } $ do
          bracket
            (do
              _ <- createSecret "default" secret
              _ <- createPod "default" pod'
              pure pod)
            (\_ -> do
              deletePod "default" "test"
              waitForPod "default" "test" isNothing
              deleteSecret "default" "test-secret"
              waitForSecret "default" "test-secret" isNothing
              )
            (\pod -> do
              runningPod <- waitForPod "default" "test" (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
              lift $ (runningPod ^. _Just.Pod.status.Pod.phase._Just) `shouldBe` "Running"
              )

    context "when a pod with the given name already exists" $
      it "throws ServantError reason 409 CONFLICT" $ do
        void $ run emptyStubConfig emptyKube $
          bracket
            (createPod "default" pod)
            (\_ -> do
                     deletePod "default" "test"
                     waitForPod "default" "test" isNothing)
            (\_ -> createPod "default" pod)
        `shouldThrow` (servantErrorWithStatusCode 409)

    context "when attached secret does not exist" $ do
      it "should not reach state 'Running'" $ do
        let pod' = pod & Pod.volumes %~ (settingsVolume <|)
            settingsVolume =  mkVolume "secret-volume"
                              & Volume.secret .~ (Just $ mkSecretVolumeSource
                              & SecretVolumeSource.secretName .~ (Just "does-not-exist"))
        void $ run emptyStubConfig emptyKube $ do
          bracket
            (do
              _ <- createPod "default" pod'
              pure pod)
            (\_ -> do
              deletePod "default" "test"
              waitForPod "default" "test" isNothing
              )
            (\pod -> do
              runningPod <- waitForPod "default" "test" (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
              lift $ (runningPod ^. _Just.Pod.status.Pod.phase._Just) `shouldBe` "Pending"
              )
        `shouldThrow` timeout

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message

timeout :: Selector Timeout
timeout Timeout = True

hasSecretVolume name pod = let
  volumes = pod ^. Pod.spec._Just.PodSpec.volumes._Just
  secretVolumeName v = v ^. Volume.secret._Just.SecretVolumeSource.secretName._Just
  in
    any (\v -> name `Text.isPrefixOf` secretVolumeName v) volumes

hasServiceAccount name pod = let
  serviceAccountName = pod ^. Pod.spec._Just.PodSpec.serviceAccountName._Just
  in
    name == serviceAccountName
