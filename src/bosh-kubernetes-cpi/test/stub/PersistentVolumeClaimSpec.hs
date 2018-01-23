{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module PersistentVolumeClaimSpec(spec) where

import           Prelude                                            hiding
                                                                     (readFile)

import           StubRunner
import           Test.Hspec

import           Control.Lens

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Maybe

import qualified CPI.Base                                           as Base
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import qualified CPI.Kubernetes.Resource.Metadata                   as Metadata
import           CPI.Kubernetes.Resource.PersistentVolumeClaim      (PVCs, PersistentVolumeClaim (..),
                                                                     createPersistentVolumeClaim,
                                                                     deletePersistentVolumeClaim,
                                                                     getPersistentVolumeClaim,
                                                                     listPersistentVolumeClaim,
                                                                     newPersistentVolumeClaim,
                                                                     waitForPersistentVolumeClaim)
import qualified CPI.Kubernetes.Resource.PersistentVolumeClaim      as PersistentVolumeClaim
import           CPI.Kubernetes.Resource.Stub.PersistentVolumeClaim
import           CPI.Kubernetes.Resource.Stub.State
import           Kubernetes.Model.V1.ObjectMeta                     (ObjectMeta, mkObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta                     as ObjectMeta
import           Network.TLS
import           Resource
import           Servant.Common.BaseUrl                             (BaseUrl, parseBaseUrl)


import qualified Data.ByteString                                    as ByteString
import qualified Data.HashMap.Strict                                as HashMap
import qualified Data.HashSet                                       as HashSet
import           Data.Text                                          (Text)
import qualified Data.Text                                          as Text

import           Data.Yaml

import           Control.Exception.Safe
import           Control.Monad.Reader

import           Control.Effect.Class.FileSystem
import           Control.Effect.Class.Wait

import           Data.Typeable
import           GHC.Stack.Types
import           Network.HTTP.Types.Status
import           Servant.Common.Req                                 (ServantError (..))
import           System.Environment

withPersistentVolumeClaim :: (PVCs m, MonadThrow m, MonadMask m, HasConfig c, MonadReader c m) => PersistentVolumeClaim -> ((PersistentVolumeClaim, PersistentVolumeClaim) -> m a) -> m a
withPersistentVolumeClaim persistentVolumeClaim action = do
  config <- asks asConfig
  let ns = namespace $ clusterAccess config
  bracket
    (do
      createdPersistentVolumeClaim <- createPersistentVolumeClaim ns persistentVolumeClaim
      pure (persistentVolumeClaim, createdPersistentVolumeClaim))
    (\(_, pvc) -> do
      deletePersistentVolumeClaim ns $ pvc ^. name
      waitForPersistentVolumeClaim "PVC to be deleted" ns (pvc ^. name) isNothing)
    action

ignoreArgument :: f -> (a -> f)
ignoreArgument f = \_ -> f

spec :: Spec
spec = describe "PersistentVolumeClaim" $ do
  describe "create" $ do
    let testPVC = newPersistentVolumeClaim "test-" "10Mi"

    it "creates a PersistentVolumeClaim with the given name prefix" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withPersistentVolumeClaim testPVC $ \(input, output) -> do
              maybePvc <- getPersistentVolumeClaim ns $ output ^. name
              lift $ maybePvc `shouldSatisfy` isJust
              let pvc = fromJust maybePvc
              lift $ (Text.unpack $ output ^. name) `shouldStartWith` (Text.unpack $ input ^. generateName)
              lift $ (Text.length $ output ^. name) `shouldSatisfy` (> (Text.length $ input ^. generateName))

    it "creates a PersistentVolumeClaim with the given size" $ do
      void $ run emptyStubConfig emptyKube $ do
        withPersistentVolumeClaim testPVC $ \(input, output) -> do
          lift $ (output ^? PersistentVolumeClaim.resourceRequests.at "capacity"._Just) `shouldBe` (Just "10Mi")

    it "creates a PersistentVolumeClaim in state 'Pending'" $ do
      void $ run emptyStubConfig emptyKube $ do
        withPersistentVolumeClaim testPVC $ \(input, output) -> do
          lift $ output ^. PersistentVolumeClaim.status.PersistentVolumeClaim.phase `shouldBe` Just "Pending"

    it "creates a PersistentVolumeClaim that will end up in state 'Bound'" $ do
      void $ run emptyStubConfig emptyKube $ do
        config <- asks asConfig
        let ns = namespace $ clusterAccess config
        withPersistentVolumeClaim testPVC $ \(input, output) -> do
          void $ waitForPersistentVolumeClaim "PVC to be bound" ns (output ^. name) (\pvc -> pvc ^. _Just.PersistentVolumeClaim.status.PersistentVolumeClaim.phase._Just == "Bound")

  describe "delete" $ do
    context "when a PersistentVolumeClaim with the given name does not exist" $
      it "throws ServantError reason 404 NOT FOUND" $ do
        void $ run emptyStubConfig emptyKube $ do
          config <- asks asConfig
          let ns = namespace $ clusterAccess config
          deletePersistentVolumeClaim ns "does-not-exist"
        `shouldThrow` (servantErrorWithStatusCode 404)

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message

timeout :: Selector Timeout
timeout (Timeout _) = True
