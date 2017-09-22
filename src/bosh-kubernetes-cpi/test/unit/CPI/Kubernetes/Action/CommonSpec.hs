module CPI.Kubernetes.Action.CommonSpec(spec) where

import           CPI.Kubernetes.Action.Common
import           Test.Hspec

import           Control.Lens

import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Pod
import qualified CPI.Kubernetes.Resource.Pod      as Pod
import qualified Kubernetes.Model.V1.Pod          as Pod

spec :: Spec
spec = describe "Common" $ do
         it "ignoringNotFound" pending
