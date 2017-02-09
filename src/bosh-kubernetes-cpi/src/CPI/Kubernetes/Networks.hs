{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module CPI.Kubernetes.Networks() where

import Data.Aeson
import Data.Aeson.Lens
import CPI.Base.Networks
import Data.Text (Text)
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Log

configure :: (MonadIO m, MonadThrow m, MonadLog Text m) =>
                NetworkSpec
             -> m ()
configure (NetworkSpec netSpec) = undefined

createServiceSpec :: Value -> [Value]
createServiceSpec netSpec = netSpec ^.. (vipNetworks . cloudProperties)

networks :: Traversal' Value Object
networks = members . _Object

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
