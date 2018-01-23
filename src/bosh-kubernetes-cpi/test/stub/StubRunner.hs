{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StubRunner(
  run
) where

import           CPI.Kubernetes.Config
import           CPI.Kubernetes.Resource.PersistentVolumeClaim
import           CPI.Kubernetes.Resource.Pod
import           CPI.Kubernetes.Resource.Secret
import           CPI.Kubernetes.Resource.Service
import           CPI.Kubernetes.Resource.Stub.PersistentVolumeClaim
import           CPI.Kubernetes.Resource.Stub.Pod
import           CPI.Kubernetes.Resource.Stub.Secret
import           CPI.Kubernetes.Resource.Stub.Service
import           CPI.Kubernetes.Resource.Stub.State
import           Resource

import Control.Effect.Stub
import Control.Effect
import Control.Effect.Class.FileSystem

import qualified Data.ByteString                                    as ByteString
import           Data.Maybe
import           Data.Yaml

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader

import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           System.Environment

type MR a = (forall c m .
              ( MonadPod (m IO)
              , MonadSecret (m IO)
              , MonadService (m IO)
              , MonadPVC (m IO)
              , MonadTrans m
              , MonadMask (m IO)
              , MonadThrow (m IO)
              , MonadReader c (m IO)
              , HasConfig c
              , MonadIO (m IO)) => (m IO) a)

run :: StubConfig -> KubeState -> MR a -> IO a
run stubConfig kubeState f = do
  maybeConfig <- loadConfig
  case maybeConfig of
    Just config -> config `runResource` f
    _ -> do (result, _, _::NoOutput) <- runStubT
                                  stubConfig
                                  kubeState
                                  f
            pure result

loadConfig :: IO (Maybe Config)
loadConfig = runMaybeT $ do
  cluster <- MaybeT $ lookupEnv "KUBE_CLUSTER"
  accessFile <- liftIO $ ByteString.readFile $ cluster
  rawAccess <- either throwM pure $ decodeEither' accessFile
  access <- liftIO $ parseClusterAccess rawAccess
  pure Config {
    clusterAccess = access
  }
