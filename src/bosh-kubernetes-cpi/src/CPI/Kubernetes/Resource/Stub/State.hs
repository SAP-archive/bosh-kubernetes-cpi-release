{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Kubernetes.Resource.Stub.State(
    KubeState(..)
  , ResourceMap
  , HasPods(..)
  , HasSecrets(..)
  , emptyKube
  , StubConfig(..)
  , HasImages(..)
  , emptyStubConfig
  , NoOutput(..)
  , StubOutput(..)
  , emptyStubOutput
  , NoInput(..)
) where

import           Prelude                       (Int, const, error)

import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashSet                  (HashSet)
import qualified Data.HashSet                  as HashSet
import           Data.Monoid
import           Data.Text                     (Text)

import           Control.Monad.Stub.Console
import           Control.Monad.Stub.FileSystem
import           Control.Monad.Stub.Wait

import           Kubernetes.Model.V1.Pod       (Pod)
import           Kubernetes.Model.V1.Secret    (Secret)

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
    pods    :: HashMap (Text, Text) Pod
  , secrets :: HashMap (Text, Text) Secret
}

emptyKube :: KubeState
emptyKube = KubeState {
    pods = HashMap.empty
  , secrets = HashMap.empty
}

class HasImages a where
  asImages :: a -> HashSet Text

data StubConfig = StubConfig {
  images :: HashSet Text
}

emptyStubConfig :: StubConfig
emptyStubConfig = StubConfig {
  images = HashSet.empty
}

instance HasImages StubConfig where
  asImages = images

instance HasStdin StubConfig

data StubOutput = StubOutput {
    waitCount :: [Int]
}

emptyStubOutput :: StubOutput
emptyStubOutput = StubOutput {
    waitCount = []
}

instance Monoid StubOutput where
  mempty = emptyStubOutput
  mappend left right = emptyStubOutput {
    waitCount = waitCount left <> waitCount right
  }

instance HasWaitCount StubOutput where
  asWaitCount n = emptyStubOutput {
    waitCount = [n]
  }

instance HasStdout StubOutput
instance HasStderr StubOutput

data NoOutput = NoOutput

instance Monoid NoOutput where
  mempty = NoOutput
  mappend _ _ = NoOutput

instance HasStdout NoOutput
instance HasStderr NoOutput
instance HasWaitCount NoOutput

data NoInput = NoInput

instance HasStdin NoInput where
  asStdin = const ""
