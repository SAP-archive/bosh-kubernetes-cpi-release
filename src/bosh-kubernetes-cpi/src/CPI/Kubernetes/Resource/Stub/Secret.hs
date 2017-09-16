{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module CPI.Kubernetes.Resource.Stub.Secret(
    Secret
  , SecretList
) where

import           CPI.Base.Errors                     (CloudError (..))
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Secret
import           CPI.Kubernetes.Resource.Servant
import           CPI.Kubernetes.Resource.Stub.State
import           Resource

import           Kubernetes.Model.Unversioned.Status (Status)
import           Kubernetes.Model.V1.DeleteOptions   (mkDeleteOptions)
import           Kubernetes.Model.V1.ObjectMeta      (ObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta      as ObjectMeta
import           Kubernetes.Model.V1.Secret          (Secret)
import qualified Kubernetes.Model.V1.Secret          as Secret
import           Kubernetes.Model.V1.SecretList      (SecretList)
import qualified Kubernetes.Model.V1.SecretList      as SecretList

import           Kubernetes.Api.ApivApi              (createNamespacedPod,
                                                      deleteNamespacedPod,
                                                      listNamespacedPod,
                                                      readNamespacedPod,
                                                      replaceNamespacedPod)

import           Control.Exception.Safe
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad.Log
import           Control.Monad.Reader
import qualified Control.Monad.State                 as State
import           Servant.Client

import           Data.Aeson
import           Data.ByteString.Lazy                (toStrict)
import           Data.HashMap.Strict                 as HashMap
import           Data.Semigroup
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.Text.Encoding                  (decodeUtf8)


import           Control.Monad.Stub.StubMonad
import           Control.Monad.Stub.Time
import           Control.Monad.Stub.Wait
import           Control.Monad.Time
import           Control.Monad.Wait
import           Data.HashMap.Strict                 (HashMap, insert)
import           Data.Hourglass.Types

import qualified GHC.Int                             as GHC

instance (MonadThrow m, MonadWait m, Monoid w, HasSecrets s, HasWaitCount w, HasTime s, HasTimeline s) => MonadSecret (StubT r s w m) where
  createSecret namespace secret = do
    secrets <- State.gets asSecrets
    let secrets' = insert (namespace, secret ^. name) secret secrets
    State.modify $ updateSecrets secrets'
    pure secret

  listSecret namespace = do
    kube <- State.get
    pure undefined

  getSecret namespace name = do
    secrets <- State.gets asSecrets
    pure $ (namespace, name) `HashMap.lookup` secrets

  updateSecret namespace pod = do
    kube <- State.get
    State.put undefined
    pure undefined

  deleteSecret namespace name = do
    timestamp <- currentTime
    State.modify $ withTimeline
                 (\events ->
                   let
                     deleted :: [s -> s]
                     deleted = [withSecrets $ HashMap.delete (namespace, name)]
                     after :: GHC.Int64 -> Elapsed
                     after n = timestamp + (Elapsed $ Seconds n)
                     in
                       HashMap.insert (after 1) deleted events
                       )
    pure undefined

  waitForSecret message namespace name predicate = waitFor (WaitConfig (Retry 20) (Seconds 1) message) (getSecret namespace name) predicate
