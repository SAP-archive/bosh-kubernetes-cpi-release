{-# LANGUAGE QuasiQuotes #-}

module CPI.Kubernetes.VmTypesSpec(spec) where

import qualified CPI.Base               as Base
import           CPI.Kubernetes.VmTypes
import           Test.Hspec

import           Data.Aeson.QQ
import           Text.RawString.QQ

import qualified Data.HashMap.Strict as HashMap
import           Control.Exception.Safe

spec :: Spec
spec =
  describe "VmTypes" $
    describe "parseVmProperties" $ do
      let vmProperties = emptyVmProperties
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
        context "given resource limits" $
          shouldParse
                [aesonQQ|
                  {
                    "resources" : {
                      "limits" : {
                        "cpu" : "100m",
                        "memory" : "3Gi"
                      }
                    }
                  }
                |]
                vmProperties {
                  resources = mempty {
                      limits = Just $ HashMap.fromList [
                          ("cpu", "100m")
                        , ("memory", "3Gi")
                      ]
                  }
                }
        context "given resource reqests" $
          shouldParse
                [aesonQQ|
                  {
                    "resources" : {
                      "requests" : {
                        "cpu" : "100m",
                        "memory" : "3Gi"
                      }
                    }
                  }
                |]
                vmProperties {
                  resources = mempty {
                      requests = Just $ HashMap.fromList [
                          ("cpu", "100m")
                        , ("memory", "3Gi")
                    ]
                  }
                }
        context "given memory limits and reqests" $
          shouldParse
                [aesonQQ|
                  {
                    "resources" : {
                      "limits" : {
                        "memory" : "3Gi"
                      },
                      "requests" : {
                        "memory" : "3Gi"
                      }
                    }
                  }
                |]
                vmProperties {
                  resources = mempty {
                      limits = Just $ HashMap.fromList [
                          ("memory", "3Gi")
                      ]
                    , requests = Just $ HashMap.fromList [
                          ("memory", "3Gi")
                    ]
                  }
                }
        context "given cpu limits and reqests" $
          shouldParse
                [aesonQQ|
                  {
                    "resources" : {
                      "limits" : {
                        "cpu" : "100m"
                      },
                      "requests" : {
                        "cpu" : "100m"
                      }
                    }
                  }
                |]
                vmProperties {
                  resources = mempty {
                      limits = Just $ HashMap.fromList [
                          ("cpu", "100m")
                      ]
                    , requests = Just $ HashMap.fromList [
                          ("cpu", "100m")
                    ]
                  }
                }
        context "given resource limits and reqests" $
          shouldParse
                [aesonQQ|
                  {
                    "resources" : {
                      "limits" : {
                        "cpu" : "100m",
                        "memory" : "3Gi"
                      },
                      "requests" : {
                        "cpu" : "100m",
                        "memory" : "3Gi"
                      }
                    }
                  }
                |]
                vmProperties {
                  resources = mempty {
                      limits = Just $ HashMap.fromList [
                          ("cpu", "100m")
                        , ("memory", "3Gi")
                      ]
                    , requests = Just $ HashMap.fromList [
                          ("cpu", "100m")
                        , ("memory", "3Gi")
                    ]
                  }
                }
        context "given resource limits and reqests as number" $
                  shouldParse
                        [aesonQQ|
                          {
                            "resources" : {
                              "limits" : {
                                "cpu" : 0.1,
                                "memory" : 1073741824
                              },
                              "requests" : {
                                "cpu" : 0.1,
                                "memory" : 1073741824
                              }
                            }
                          }
                        |]
                        vmProperties {
                          resources = mempty {
                              limits = Just $ HashMap.fromList [
                                  ("cpu", "0.1")
                                , ("memory", "1.0737418e9")
                              ]
                            , requests = Just $ HashMap.fromList [
                                  ("cpu", "0.1")
                                , ("memory", "1.0737418e9")
                            ]
                          }
                        }
      context "given an invalid service spec with unknown service type" $
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

shouldParse raw parsed =
  it "shouldParseSuccessfully" $ do
    result <- Base.parseArgument raw
    result `shouldBe` parsed