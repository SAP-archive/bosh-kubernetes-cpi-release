{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module CPI.Kubernetes.Resource.Secret(
    Secret
  , SecretList
  , MonadSecret(..)
  , name
) where

import qualified CPI.Base                            as Base
import           CPI.Base.Errors                     (CloudError (..))
import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.Servant
import           Resource

import           Kubernetes.Model.Unversioned.Status (Status)
import           Kubernetes.Model.V1.DeleteOptions   (mkDeleteOptions)
import           Kubernetes.Model.V1.ObjectMeta      (ObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta      as ObjectMeta
import           Kubernetes.Model.V1.Secret          (Secret)
import qualified Kubernetes.Model.V1.Secret          as Secret
import           Kubernetes.Model.V1.SecretList      (SecretList)
import qualified Kubernetes.Model.V1.SecretList      as SecretList

import           Kubernetes.Api.ApivApi              (createNamespacedSecret,
                                                      deleteNamespacedSecret,
                                                      listNamespacedSecret,
                                                      readNamespacedSecret,
                                                      replaceNamespacedSecret)

import           Control.Monad.Reader

import           Control.Exception.Safe
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad.Console
import           Control.Monad.FileSystem
import           Control.Monad.Log
import           Data.Aeson
import           Data.ByteString.Lazy                (toStrict)
import           Data.Semigroup
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.Text.Encoding                  (decodeUtf8)
import           Servant.Client

class (Monad m) => MonadSecret m where
  createSecret :: Text -> Secret -> m Secret
  listSecret :: Text -> m SecretList
  getSecret :: Text -> Text -> m (Maybe Secret)
  updateSecret :: Text -> Secret -> m Secret
  deleteSecret :: Text -> Text -> m Status
  waitForSecret :: Text -> Text -> (Secret -> Bool) -> m Secret

instance (MonadIO m, MonadThrow m, MonadCatch m, MonadConsole m, MonadFileSystem m, HasConfig c) => MonadSecret (Resource c m) where

  createSecret namespace secret = do
    logDebug $ "Creating secret '" <> (decodeUtf8.toStrict.encode) secret <> "'"
    restCall $ createNamespacedSecret namespace Nothing secret

  listSecret namespace = do
    logDebug $ "List secrets in '" <> namespace <> "'"
    restCall $ listNamespacedSecret namespace Nothing Nothing Nothing Nothing Nothing Nothing

  getSecret namespace name = do
    logDebug $ "Get secret '" <> namespace <> "/" <> name <> "'"
    restCallGetter $ readNamespacedSecret namespace name Nothing Nothing Nothing

  updateSecret namespace secret = do
    logDebug $ "Update secret '" <> (decodeUtf8.toStrict.encode) secret <> "'"
    restCall $ replaceNamespacedSecret namespace (secret ^. Secret.metadata._Just.ObjectMeta.name._Just) Nothing secret

  deleteSecret namespace name = do
    logDebug $ "Delete secret '" <> namespace <> "/" <> name <> "'"
    restCall $ deleteNamespacedSecret namespace name Nothing (mkDeleteOptions 0)

  waitForSecret namespace name f = do
    logDebug $ "Waiting for secret '" <> namespace <> "/" <> name <> "'"
    mSecret <- getSecret namespace name
    case mSecret of
      Just secret -> pure $ if f secret then secret else secret
      Nothing     -> throwM $ CloudError ""

name :: Secret -> Text
name secret = secret ^. Secret.metadata._Just.ObjectMeta.name._Just
