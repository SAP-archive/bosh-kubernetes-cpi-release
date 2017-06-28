module CPI.Kubernetes.Base64Spec(spec) where

import           CPI.Kubernetes.Base64
import           Test.Hspec
import           Test.QuickCheck           (property)
import           Test.QuickCheck.Instances

import           Data.Aeson                (Result (..), Value (..),
                                            eitherDecode, fromJSON, toJSON)
import qualified Data.Aeson                as Aeson

import           Data.ByteString           (ByteString)
import           Data.ByteString.Lazy      (fromStrict)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                 (Text)
import           Data.Text.Encoding


import           Control.Applicative

spec :: Spec
spec = describe "Base64" $ do
  let decoded = String "The quick brown fox jumps over the lazy dog."
      encoded = "IlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3ZlciB0aGUgbGF6eSBkb2cuIg=="
  it "decodeJSON reverses encodeJSON" $ property $ do
    \x -> (fromJust . decodeJSON . encodeJSON) x == (x::Text)
  it "can encode json content" $ do
    encodeJSON decoded `shouldBe` encoded
  it "can decode json content" $ do
    (fromJust $ decodeJSON $ encoded) `shouldBe` decoded
