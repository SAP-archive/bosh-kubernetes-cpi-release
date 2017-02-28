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
                  "name": "my-service",
                  "type": "NodePort",
                  "ports": [
                    {
                      "name": "my-port",
                      "port": 22,
                      "nodePort": 30022
                    }
                  ]
                }
              ]
            }
          |]
          result <- parseVmProperties service
          result `shouldBe` vmProperties {
            _services = [Service {
                _serviceName = "my-service"
              , _serviceType = NodePort
              , _servicePorts = [ServicePort {
                  _portName = "my-port"
                , _port = 22
                , _nodePort = 30022
              }]
            }]
          }
      context "given an invalid service spec with unknown service type" $ do
        it "should fail" $ do
          let service = Base.VmProperties [aesonQQ|
            {
              "services": [
                {
                  "name": "my-service",
                  "type": "UnknownType"
                }
              ]
            }
          |]
          result <- try (parseVmProperties service)
          let Left (Base.CloudError errorMessage) = result
          errorMessage `shouldBe` "Could not parse 'VmProperties': failed to parse field services: expected one of [NodePort], encountered '\"UnknownType\"'"
