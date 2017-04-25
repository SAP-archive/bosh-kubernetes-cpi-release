{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Kubernetes.Resource.Stub.State(
    KubeState(..)
  , ResourceMap(..)
  , HasTicks(..)
  , HasPods(..)
  , HasSecrets(..)
  , emptyKube
  , StubConfig(..)
) where

import           CPI.Base
import           Prelude                       (Applicative, IO, error, (.))

import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text                     (Text)

import           Control.Exception.Safe
import           Control.Monad.Arguments
import           Control.Monad.Console
import           Control.Monad.FileSystem
import           Control.Monad.Reader
import qualified Control.Monad.State           as State


import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.StubMonad

import           Kubernetes.Model.V1.Pod       (Pod)
import qualified Kubernetes.Model.V1.Pod       as Pod
import           Kubernetes.Model.V1.Secret    (Secret, mkSecret)
import qualified Kubernetes.Model.V1.Secret    as Secret

class HasTicks a r where
  asTicks :: a -> [r -> r]

type ResourceMap r = HashMap (Text, Text) r

class HasPods a where
  asPods :: a -> ResourceMap Pod
  updatePods :: ResourceMap Pod -> a -> a

instance HasPods KubeState where
  asPods = pods
  updatePods newPods s = s {
    pods = newPods
  }

class HasSecrets a where
  asSecrets :: a -> HashMap (Text, Text) Secret
  updateSecrets :: HashMap (Text, Text) Secret -> a -> a

instance HasSecrets KubeState where
  asSecrets = secrets
  updateSecrets ss s = s {
    secrets = ss
  }

instance HasFiles KubeState where
  asFiles = error "No file system available"

data KubeState = KubeState {
    stubConfig :: StubConfig
  , pods       :: HashMap (Text, Text) Pod
  , secrets    :: HashMap (Text, Text) Secret
}

emptyKube = KubeState {
    stubConfig = StubConfig
  , pods = HashMap.empty
  , secrets = HashMap.empty
}

data StubConfig = StubConfig
