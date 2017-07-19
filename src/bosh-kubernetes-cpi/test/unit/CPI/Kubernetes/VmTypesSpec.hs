{-# LANGUAGE QuasiQuotes #-}

module CPI.Kubernetes.VmTypesSpec(spec) where

import qualified CPI.Base               as Base
import           CPI.Kubernetes.VmTypes
import           Test.Hspec

import           Data.Aeson.QQ
import           Text.RawString.QQ

import           Control.Exception.Safe

spec :: Spec
spec = do
  describe "VmTypes" $ do
    describe "parseVmProperties" $ do
      let vmProperties = VmProperties {
        _services = []
      }
      it "should parse empty cloud_properties" $ do
        let empty = Base.VmProperties [aesonQQ|
          {}
        |]
        result <- parseVmProperties empty
        result `shouldBe` vmProperties
      context "given a service of type NodePort" $ do
        it "should parse the list of ports" $ do
          let service = Base.VmProperties [aesonQQ|
            {
              "services": [
                {
                  "name": "my-service"
                }
              ]
            }
          |]
          result <- parseVmProperties service
          result `shouldBe` vmProperties {
            _services = [Service {
                _serviceName = "my-service"
            }]
          }
      context "given an invalid service spec with unknown service type" $ do
        it "should fail" $ do
          let service = Base.VmProperties [aesonQQ|
            {
              "services": [
                {
                  "unknown-key": "value"
                }
              ]
            }
          |]
          result <- try (parseVmProperties service)
          let Left (Base.CloudError errorMessage) = result
          errorMessage `shouldBe` "Could not parse 'VmProperties': failed to parse field services: When parsing the record Service of type CPI.Kubernetes.VmTypes.Service the key name was not present."
