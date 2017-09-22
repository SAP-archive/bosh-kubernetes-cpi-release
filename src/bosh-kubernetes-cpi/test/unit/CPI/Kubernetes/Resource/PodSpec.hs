module CPI.Kubernetes.Resource.PodSpec(spec) where

import           Test.Hspec

import           Control.Lens

import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.Pod
import qualified CPI.Kubernetes.Resource.Pod      as Pod
import qualified Kubernetes.Model.V1.Pod          as Pod

import qualified Data.HashMap.Strict              as HashMap

spec :: Spec
spec = describe "Pod" $ do
         describe "copyPod" $ do
           it "copies metadata.name" $ do
             let pod = newPod "pod-name" $ newContainer "container-name" "repo/image"

             let pod' = newPodFrom pod

             pod' ^. name `shouldBe` (pod ^. name)
           it "copies metadata.labels" $ do
             let pod = (newPod "pod-name" $ newContainer "container-name" "repo/image")
                                         & labels .~ HashMap.singleton "key" "value"

             let pod' = newPodFrom pod

             pod' ^. labels `shouldBe` (pod ^. labels)
           it "copies spec" $ do
             let pod = newPod "pod-name" $ newContainer "container-name" "repo/image"

             let pod' = newPodFrom pod

             pod' ^. Pod.spec `shouldBe` (pod ^. Pod.spec)

           it "copies volumes" $ do
             let pod = (newPod "pod-name" $ newContainer "container-name" "repo/image")
                                          & volumes .~ [newPersistentVolume "volume" "claim"]

             let pod' = newPodFrom pod

             pod' ^. volumes `shouldBe` (pod ^. volumes)
             pod' ^. volumes `shouldBe` [newPersistentVolume "volume" "claim"]
