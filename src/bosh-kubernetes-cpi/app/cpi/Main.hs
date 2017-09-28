{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import qualified CPI.Base       as Base
import CPI.Kubernetes (Config)
import Resource (Resource)

main :: IO ()
main = Base.runRequest (Base.handleRequest :: Base.Request -> Resource Config IO Base.Response)
