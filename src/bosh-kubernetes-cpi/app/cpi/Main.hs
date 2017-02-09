{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import qualified CPI.Base       as Base
import qualified CPI.Kubernetes as Kubernetes

main :: IO ()
main = Base.run (Base.handleRequest :: Base.Request -> Base.CpiMonad Kubernetes.Config Base.Response)
