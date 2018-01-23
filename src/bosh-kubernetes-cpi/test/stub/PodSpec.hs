{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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
import           CPI.Kubernetes.Resource.Pod            (Pods, createPod,
                                                         deletePod, getPod,
                                                         newContainer, newPod,
                                                         waitForPod)
import qualified CPI.Kubernetes.Resource.Pod            as Pod
import           CPI.Kubernetes.Resource.Secret
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.State     (emptyKube,
                                                         emptyStubConfig)
import qualified CPI.Kubernetes.Resource.Stub.State     as State

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

import qualified Data.HashMap.Strict                    as HashMap
import qualified Data.HashSet                           as HashSet
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Network.TLS
import           Resource
import           Servant.Common.BaseUrl                 (BaseUrl, parseBaseUrl)

import           Control.Exception.Safe
import           Control.Monad.Reader

import           Control.Effect.Class.FileSystem
import           Control.Effect.Class.Wait

import           Data.Typeable
import           GHC.Stack.Types
import           Network.HTTP.Types.Status
import           Servant.Common.Req                     (ServantError (..))
import           System.Environment

withPod :: (Pods m, MonadThrow m, MonadMask m, HasConfig c, MonadReader c m) => Pod -> (Pod -> m a) -> m a
withPod pod action = do
  config <- asks asConfig
  let ns = namespace $ clusterAccess config
  bracket
    (do
      createPod ns pod
    )
    (\pod' -> do
      let podName = pod' ^. name
      deletePod ns podName
      waitForPod "Pod to be deleted" ns podName isNothing
    )
    (\pod' -> do
      action pod
    )

spec :: Spec
spec =
  describe "create" $ do
    let pod = newPod "test" container
        container = newContainer "busybox" "busybox"
    it "creates a pod with the given name" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withPod pod $ do
          (\createdPod -> do
            let createdPodName = createdPod ^. Pod.metadata._Just.ObjectMeta.name._Just
            lift $ createdPodName `shouldBe` "test"
            pod' <- getPod ns createdPodName
            lift $ pod' `shouldSatisfy` isJust
            lift $ (pod ^. name) `shouldBe` (createdPod ^. name)
            )

    it "creates a pod with default service account" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withPod pod $ do
          (\pod' -> do
              lift $ (pod' ^. name) `shouldBe` "test"
              maybePod <- waitForPod "Pod to have default service account" ns (pod' ^. name) (maybe False (hasServiceAccount "default"))
              lift $ maybePod `shouldSatisfy` isJust
              let Just pod'' = maybePod
              lift $ pod'' `shouldSatisfy` hasServiceAccount "default"
              lift $ pod'' `shouldSatisfy` hasSecretVolume "default-token"
              lift $ (pod'' ^.. Pod.container.Container.volumeMounts._Just.each.VolumeMount.mountPath) `shouldBe` ["/var/run/secrets/kubernetes.io/serviceaccount"]
              lift $ (pod'' ^.. Pod.container.Container.volumeMounts._Just.each.VolumeMount.readOnly) `shouldBe` [Just True]
              )

    it "creates a pod in state 'Pending'" $ do
      void $ run emptyStubConfig emptyKube $ do
        withPod pod $ do
          (\pod' -> do
            config <- asks asConfig
            let ns = namespace $ clusterAccess config
            maybePendingPod <- waitForPod "Pod to be pending" ns (pod' ^. name) (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Pending")
            lift $ maybePendingPod `shouldSatisfy` isJust
            )

    it "creates a pod that will end up in state 'Running'" $ do
      void $ run
              emptyStubConfig
              emptyKube {
                  State.images = HashSet.singleton "busybox"
                , State.secrets = HashMap.singleton ("default", "default-token") (newSecret "default-token")
              } $ do
        withPod pod $ do
          (\pod' -> do
            config <- asks asConfig
            let ns = namespace $ clusterAccess config
            runningPod <- waitForPod "Pod to be running" ns (pod' ^. name) (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
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
                    State.images = HashSet.singleton "busybox"
                  , State.secrets = HashMap.singleton ("default", "default-token") (newSecret "default-token")
                } $ do
            withPod pod $ do
              (\pod' -> do
                config <- asks asConfig
                let ns = namespace $ clusterAccess config
                runningPod <- waitForPod "Pod to be running" ns (pod' ^. name) (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
                lift $ (runningPod ^. _Just.Pod.status.Pod.phase._Just) `shouldBe` "Running"
                )

    it "creates a Pod with resource requirements" $ do
      let pod' = pod & Pod.container.Pod.resources.Pod.limit "cpu" .~ "1"
      void $ run emptyStubConfig emptyKube $ do
              config <- asks asConfig
              let ns = namespace $ clusterAccess config
              withPod pod' $ do
                (\pod' -> do
                    lift $ (pod' ^. name) `shouldBe` "test"
                    maybePod <- waitForPod "Pod to have default service account" ns (pod' ^. name) (maybe False (hasServiceAccount "default"))
                    lift $ maybePod `shouldSatisfy` isJust
                    let Just pod'' = maybePod
                    lift $ (pod'' ^.. Pod.container.Pod.resources.Pod.limit "cpu") `shouldBe` ["1"]
                    )

    context "when a pod with the given name already exists" $
      it "throws ServantError reason 409 CONFLICT" $ do
        void $ run emptyStubConfig emptyKube $
          withPod pod $ do
            (\pod' -> do
              config <- asks asConfig
              let ns = namespace $ clusterAccess config
              createPod ns pod)
        `shouldThrow` (servantErrorWithStatusCode 409)

    context "when attached secret does not exist" $ do
      it "should not reach state 'Running'" $ do
        let pod' = pod & Pod.volumes %~ (settingsVolume <|)
            settingsVolume =  mkVolume "secret-volume"
                              & Volume.secret .~ (Just $ mkSecretVolumeSource
                              & SecretVolumeSource.secretName .~ (Just "does-not-exist"))
        void $ run emptyStubConfig emptyKube $ do
          withPod pod' $ do
            (\pod' -> do
              config <- asks asConfig
              let ns = namespace $ clusterAccess config
              waitForPod "Pod to be running" ns (pod' ^. name) (\pod -> pod ^. _Just.Pod.status.Pod.phase._Just == "Running")
              )
        `shouldThrow` timeout

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message

timeout :: Selector Timeout
timeout (Timeout _) = True

hasSecretVolume name pod = let
  volumes = pod ^. Pod.spec._Just.PodSpec.volumes._Just
  secretVolumeName v = v ^. Volume.secret._Just.SecretVolumeSource.secretName._Just
  in
    any (\v -> name `Text.isPrefixOf` secretVolumeName v) volumes

hasServiceAccount name pod = let
  serviceAccountName = pod ^. Pod.spec._Just.PodSpec.serviceAccountName._Just
  in
    name == serviceAccountName
