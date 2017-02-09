{-# LANGUAGE OverloadedStrings #-}
module CPI.Base.AgentConfig(
  addPersistentDisk
) where

import           Control.Lens
import           CPI.Base.Inputs
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

addPersistentDisk :: Text -> Text -> Value -> Value
addPersistentDisk diskId path settings =
  let empty :: Value
      empty = Object HashMap.empty in
    settings & _Object
            .at "disks".non empty._Object
            .at "persistent".non empty._Object
            .at diskId.non empty._Object
            .at "path"._Just._String
            .~ path
  -- settings .at "disks".at "persistent".at diskId .~

-- Example settings.json
-- {
--   "agent_id": "agent-xxxxxx",
--   "blobstore": {
--     "provider": "local",
--     "options": {
--       "endpoint": "http://xx.xx.xx.xx:25250",
--       "password": "password",
--       "blobstore_path": "/var/vcap/micro_bosh/data/cache",
--       "user": "agent"
--     }
--   },
--   "disks": {
--     "system": "/dev/xvda",
--     "ephemeral": "/dev/sdb",
--     "persistent": {}
--   },
--   "env": {},
--   "networks": {
--     "default": {
--       "type": "manual",
--       "ip": "10.234.228.158",
--       "netmask": "255.255.255.192",
--       "cloud_properties": {"name": "3112 - preprod - back"},
--       "dns": [
--         "10.234.50.180",
--         "10.234.71.124"
--       ],
--       "gateway": "10.234.228.129",
--       "mac": null
--     }
--   },
--   "ntp": [],
--   "mbus": "nats://nats:nats-password@yy.yy.yyy:4222",
--   "vm": {"name": "vm-yyyy"},
--   "trusted_certs": null
-- }


-- makeLenses ''PublicKeys
-- instance ToJSON PublicKeys where
--   toJSON (PublicKeys keys) =
--       object ["public-keys" .= fmap createEntry indexedKeys]
--       where
--         indexedKeys :: [(Int,Text)]
--         indexedKeys = zip [0..(length keys)] keys
--         createEntry :: (Int, Text) -> Value
--         createEntry (index, key) = object [(Text.pack . show) index .= object ["openssh-key" .= key]]
