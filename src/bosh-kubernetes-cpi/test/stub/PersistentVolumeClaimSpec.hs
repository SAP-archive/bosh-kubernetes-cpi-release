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
import           CPI.Kubernetes.Resource.PersistentVolumeClaim      (MonadPVC, PersistentVolumeClaim (..),
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
import           Control.Monad.FileSystem
import           Control.Monad.Stub.StubMonad
import           Control.Monad.Wait
import           Data.Typeable
import           GHC.Stack.Types
import           Network.HTTP.Types.Status
import           Servant.Common.Req                                 (ServantError (..))
import           System.Environment

withPersistentVolumeClaim :: (MonadPVC m, MonadThrow m, MonadMask m) => Text -> PersistentVolumeClaim -> ((PersistentVolumeClaim, PersistentVolumeClaim) -> m a) -> m a
withPersistentVolumeClaim namespace persistentVolumeClaim action =
  bracket
    (do
      createdPersistentVolumeClaim <- createPersistentVolumeClaim "default" persistentVolumeClaim
      pure (persistentVolumeClaim, createdPersistentVolumeClaim))
    (\(_, pvc) -> do
      deletePersistentVolumeClaim "default" $ pvc ^. name
      waitForPersistentVolumeClaim "default" (pvc ^. name) isNothing)
    action

ignoreArgument :: f -> (a -> f)
ignoreArgument f = \_ -> f

spec :: Spec
spec =
  describe "create" $ do
    let testPVC = newPersistentVolumeClaim "test-" "10Mi"

    it "creates a PersistentVolumeClaim with the given name prefix" $ do
      void $ run emptyStubConfig emptyKube $ do
        withPersistentVolumeClaim "my-pvc" testPVC $ \(input, output) -> do
              maybePvc <- getPersistentVolumeClaim "default" $ output ^. name
              lift $ maybePvc `shouldSatisfy` isJust
              let pvc = fromJust maybePvc
              lift $ (Text.unpack $ output ^. name) `shouldStartWith` (Text.unpack $ input ^. generateName)
              lift $ (Text.length $ output ^. name) `shouldSatisfy` (> (Text.length $ input ^. generateName))

    it "creates a PersistentVolumeClaim with the given size" $ do
      void $ run emptyStubConfig emptyKube $ do
        withPersistentVolumeClaim "my-pvc" testPVC $ \(input, output) -> do
          lift $ (output ^? PersistentVolumeClaim.resourceRequests.at "capacity"._Just) `shouldBe` (Just "10Mi")

    it "creates a PersistentVolumeClaim in state 'Pending'" $ do
      void $ run emptyStubConfig emptyKube $ do
        withPersistentVolumeClaim "my-pvc" testPVC $ \(input, output) -> do
          lift $ output ^. PersistentVolumeClaim.status.PersistentVolumeClaim.phase `shouldBe` Just "Pending"

    it "creates a PersistentVolumeClaim that will end up in state 'Bound'" $ do
      void $ run emptyStubConfig emptyKube $ do
        withPersistentVolumeClaim "my-pvc" testPVC $ \(input, output) -> do
          void $ waitForPersistentVolumeClaim "default" (output ^. name) (\pvc -> pvc ^. _Just.PersistentVolumeClaim.status.PersistentVolumeClaim.phase._Just == "Bound")

servantErrorWithStatusCode :: Int -> Selector ServantError
servantErrorWithStatusCode expectedStatusCode (FailureResponse (Status code _) _ _) = expectedStatusCode == code

cloudErrorWithMessage :: Text -> Selector Base.CloudError
cloudErrorWithMessage expectedMessage (Base.CloudError message) = expectedMessage == message

timeout :: Selector Timeout
timeout Timeout = True
