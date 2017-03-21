{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CPI.Kubernetes.ConfigSpec(spec) where

import           CPI.Base.System
import           CPI.Kubernetes.Config
import           Data.Aeson.QQ
import           Test.Hspec
import           Text.RawString.QQ

import           Data.Aeson
import           Data.ByteString.Lazy   (fromStrict, toStrict)


import           Control.Exception.Safe
import           Control.Monad.State
import           Data.ByteString        (ByteString)
import           Data.HashMap.Strict    (HashMap, (!))
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import qualified Servant.Common.BaseUrl as Url


spec :: Spec
spec =
  describe "parseConfig" $ do
    context "given an explicit access" $ do
      let rawConfig :: (ToJSON a, ?creds :: a) => ByteString
          rawConfig = rawConfig' ?creds
          rawConfig' creds =  toStrict $ encode [aesonQQ|
        {
          "access": {
            "server": "https://my.kubernetes.io:4443",
            "namespace": "default",
            "credentials": #{creds}
          },
          "agent": {}
        }
      |]
      let ?creds = [aesonQQ|{"token": "xxxxx-xxxxx-xxxxx-xxxxx"}|]
      it "should parse namespace" $ do
        config <- parseConfig rawConfig
        namespace' <- namespace $ clusterAccess config
        namespace' `shouldBe` "default"
      it "should parse server" $ do
        config <- parseConfig rawConfig
        server' <- server $ clusterAccess config
        expectedServer <- Url.parseBaseUrl "https://my.kubernetes.io:4443"
        server' `shouldBe` expectedServer
      context "with valid client certs" $ do
        let ?creds = [aesonQQ|
          {
            "certificate": "certificate",
            "private_key": #{privateKey}
          }
        |]
        it "should parse credentials" $ do
          config <- parseConfig rawConfig
          ClientCertificate credentials' <- credentials $ clusterAccess config
          let (certChain, privateKey) = credentials'
          1 `shouldBe` 1
      context "with a token" $ do
        let ?creds = [aesonQQ| { "token": "xxxxx-xxxxx-xxxxx-xxxxx" } |]
        it "should parse credentials" $ do
          config <- parseConfig rawConfig
          Token token <- credentials $ clusterAccess config
          token `shouldBe` "xxxxx-xxxxx-xxxxx-xxxxx"
    context "given access of type ServiceAccount" $ do
      let rawConfig = toStrict $ encode [aesonQQ|
        {
          "access": "ServiceAccount",
          "agent": {}
        }
      |]
      let fileSystem = SystemState $ HashMap.fromList [
              ("/var/run/secrets/kubernetes.io/serviceaccount/namespace", "default")
            , ("/var/run/secrets/kubernetes.io/serviceaccount/token", "xxxxx-xxxxx-xxxxx-xxxxx")]
      it "should read namespace from service account" $ do
        config <- parseConfig rawConfig
        namespace' <- runTestFileSystem fileSystem $
            namespace $ clusterAccess config
        namespace' `shouldBe` "default"
      it "should use 'https://kubernetes' as server url" $ do
        config <- parseConfig rawConfig
        server' <- server $ clusterAccess config
        expectedServer <- Url.parseBaseUrl "https://kubernetes"
        server' `shouldBe` expectedServer
      it "should read token from service account" $ do
        config <- parseConfig rawConfig
        Token token <- runTestFileSystem fileSystem $
            credentials $ clusterAccess config
        token `shouldBe` "xxxxx-xxxxx-xxxxx-xxxxx"

runTestFileSystem :: s -> TestFileSystem s b -> IO b
runTestFileSystem content f = do
  result <- unTestSystem f `evalStateT` content
  pure result

newtype TestFileSystem state a = TestFileSystem {
  unTestSystem :: StateT state IO a
} deriving (Functor, Applicative, Monad, MonadState state, MonadThrow, MonadCatch)

data SystemState = SystemState {
  fileSystem :: HashMap Text ByteString
}

instance FileSystem (TestFileSystem SystemState) where
  readFile path = do
    fs <- gets fileSystem
    pure $ fs ! path

privateKey :: Text
privateKey = [r|
-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAu5sGJ7lKDtJBw5xjpJY5Nn5vg69k6vGOowsDzFRdP0+9JWNq
5Aee+fUoHgRyf0WUi13GH4cjs9sN9DUN1JeKufPkt0rD1w8EFOZxjPt6apRh2SYC
x+I0pz44tEg4OFNSBHO3F8gTLw7K8fiT/OuDEYcNgjew6jfBVVohbVCErfEDnB/G
n5+WxH5clLcqMaZfuC4mJA8tx49msvCADqeoz/DROeYMpA9+l8+4PFMX6RCM96Of
f9NpXsnbug221UzxHZIUEV97Mr/l0Y7rsxwUqaB8pNpJ648+qei5VVa66oMeC8b7
0hZcBcgspiB6FHSK0q9UhD0oSrfxYJOVxM3gqQIDAQABAoIBABgOpRdq90g3Rh+j
alOsv+FxDTPBxhsqprPZsb7+Aocf3o1w1kAvif9bpK1UvKn9bjMA72sTlUx3Bq8O
LpvYYv29fNLUT5DAaDGV63G8vdH0/ScvbKPdKgtYO0VDDZKLfLT9cbkm+u7J4tRs
n+2K9d/FhcHxCkq+o5giWq796EW1razrypIycYKzW/wmbxtI61zte0799eUhjZFg
YK+YYgo08YNyPsyuD/D8KTxa1uoS85VTi5INSW7u+hqbRZAm/BrGCvrZUQ7+piwR
Ep+kIBJnbD1nBuJcyX1QJI1N2i35aUDdSAqM72dQH1OMWBi7daNNUchGC3/E+ZSN
3eoCnkECgYEAwRu/6SFeoumO6ay365gd2PhInXI82bBwXyoNhvBXpaj654ROQfg0
Iu2p830ZaTjSv2xGC7tUmZU1ttQQaymLgUtYcSN86D6/IKCFQs9/zUZ22ZwV1hwq
Jj23wIGkqH4O3QXhduH8YLLiU0O3OjzHbIlvnn18x8oHQwFsVeN+8iUCgYEA+LR6
7j9ZkxL1Vmi3IkdoZmdo/9im7IxaxiK4Q5FNpb0bVBHWmKAC8HfxjOzqYeWs4GwT
1SaOTUhonP21KDsnDJWaFezmuDqPqTS3VMsJGMPNcK+4QkxMPkDDnl9hbNbWuszq
uyoxURirZ3e+jYxEQgZjA3jtWGfRqRXwtXdJEzUCgYBAKIYUYL/ehJa00Guy3LFd
+u+1T9UjxlkvZPtlj8ivA3uJHA4cIOjBihDjEvc0XGq4qrKDB1ROSqK0AbUKxZzR
8kSKIm5Hg0FhB7P+xI4Dl5u5JQCkSGtAlVTNosUgLfGmQWPtaZu+TPChFWh08uiX
CPqKv8qLXnYXLwvdZV4x+QKBgH6IXE7gfjM8nwOibSIMkIohLKOWV37b/cb2nScL
QyUCrGe+V575MeWkMInRc4HxN15KvmBgqF+balYNImDgj4Jwjp9/EvdCHBsrTebf
Eba+z8P4MtfQN64ohx4JSujz+PW7EeW9lq+6zGHs407iwUuSMkfu+1pSH7JWDkxU
7yHNAoGAYZbfzOfkMX82XXHmLenv7ePuFXhjW3cUNNAiOKoQs5Mcv0LHxKJiOr74
GCEfLNbK8p+Gu5RbYnQnTvgcw3/nMcxULwB4rCTJqfQ7Il4z1E0hd7ZmXH37Ixy+
/RPv094xkEoN9uPaJJj0vQEYxkC50ZCzf4gN+qHyH9jdiQ3JoKw=
-----END RSA PRIVATE KEY-----
|]
