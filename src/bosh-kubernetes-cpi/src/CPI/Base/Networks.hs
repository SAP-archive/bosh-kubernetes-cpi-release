{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module CPI.Base.Networks(NetworkSpec(..)) where

import Data.Aeson

newtype NetworkSpec = NetworkSpec Object
