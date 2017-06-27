module CPI.Kubernetes.Resource.Metadata(
    name
  , labels
) where

import           Control.Lens

import           Data.Aeson.Types
import qualified Data.HashMap.Strict            as HashMap
import           Data.Maybe
import           Data.Text

import           Kubernetes.Model.V1.Any        (Any)
import qualified Kubernetes.Model.V1.Any        as Any
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta as ObjectMeta
import           Kubernetes.Model.V1.Pod        (Pod)
import qualified Kubernetes.Model.V1.Pod        as Pod
import           Kubernetes.Model.V1.Secret     (Secret)
import qualified Kubernetes.Model.V1.Secret     as Secret

class HasMetadata m where
  metadata :: Traversal' m (Maybe ObjectMeta)

instance HasMetadata Pod where
  metadata = Pod.metadata
instance HasMetadata Secret where
  metadata = Secret.metadata

name :: (HasMetadata m) => Traversal' m Text
name = metadata._Just.ObjectMeta.name._Just

labels :: (HasMetadata m) => Traversal' m Object
labels = metadata._Just.ObjectMeta.labels.non (Any.Any HashMap.empty).Any.any
