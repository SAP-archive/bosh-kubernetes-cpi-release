{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module CPI.Base.Inputs(
    AgentId
  , StemcellId
  , VolumeId
  , VmId
  , DiskId
  , VmProperties(..)
  , StemcellProperties(..)
  , DiskProperties(..)
  , DiskLocality
  , Environment
  , Networks
) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Text           (Text)

import           Data.HashMap.Strict (HashMap)



-- dynamicNetwork :: (Object -> f Object) -> Value -> f Value
-- dynamicNetwork = networks.filtered (anyOf (== "dynamic") (view networkType))

type AgentId = Text
type StemcellId = Text
type VolumeId = Text
type VmId = Text
type DiskId = Text
newtype VmProperties = VmProperties Value deriving (FromJSON, ToJSON)
newtype StemcellProperties = StemcellProperties Value deriving (FromJSON, ToJSON)
newtype DiskProperties = DiskProperties Value deriving (Show, FromJSON, ToJSON)
type DiskLocality = [VolumeId]
newtype Environment = Environment Value deriving (FromJSON, ToJSON)
newtype Networks = Networks Value deriving (Show, FromJSON, ToJSON)
