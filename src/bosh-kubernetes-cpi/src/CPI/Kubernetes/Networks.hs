{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module CPI.Kubernetes.Networks where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text            (Text)

-- configure :: (MonadIO m, MonadThrow m, MonadLog Text m) =>
--                 NetworkSpec
--              -> m ()
-- configure (NetworkSpec netSpec) = undefined

createServiceSpec :: Value -> [Value]
createServiceSpec netSpec = netSpec ^.. (vipNetworks . cloudProperties)

networks :: Traversal' Value Value
networks = members

preconfigured :: Traversal' Object Value
preconfigured = at "preconfigured".non (Bool False)

networkType :: Traversal' Value Text
networkType = key "type" . _String

dynamicNetworks :: Traversal' Value Value
dynamicNetworks = members . filtered isDynamic

vipNetworks :: Traversal' Value Value
vipNetworks = members . filtered isVip

isDynamic :: Value -> Bool
isDynamic network = network ^? networkType == Just "dynamic"

isVip :: Value -> Bool
isVip network = network ^? networkType == Just "vip"

cloudProperties :: Traversal' Value Value
cloudProperties = key "cloudProperties"
