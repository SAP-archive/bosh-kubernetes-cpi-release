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
        services = []
      }
      it "should parse empty cloud_properties" $ do
        let empty = [aesonQQ|
          {}
        |]
        result <- Base.parseArgument empty
        result `shouldBe` vmProperties
      context "given a service name" $ do
        it "should parse successfully" $ do
          let service = [aesonQQ|
            {
              "services": [
                {
                  "name": "my-service"
                }
              ]
            }
          |]
          result <- Base.parseArgument service
          result `shouldBe` vmProperties {
            services = [Service {
                serviceName = "my-service"
            }]
          }
      context "given an invalid service spec with unknown service type" $ do
        it "should fail" $ do
          let service = [aesonQQ|
            {
              "services": [
                {
                  "unknown-key": "value"
                }
              ]
            }
          |]

          result :: Either Base.CloudError VmProperties <- try (Base.parseArgument service)
          let Left (Base.CloudError errorMessage) = result
          errorMessage `shouldBe` "Could not parse value '{\"services\":[{\"unknown-key\":\"value\"}]}': 'failed to parse field services: When parsing the record Service of type CPI.Kubernetes.VmTypes.Service the key name was not present.'"
