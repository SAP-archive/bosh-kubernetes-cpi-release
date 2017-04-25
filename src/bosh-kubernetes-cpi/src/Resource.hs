{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}

module Resource(
    Resource
  , runResource
) where

import           Prelude                      hiding (readFile)

import           CPI.Base.System

import           Control.Exception.Safe
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Text                    (Text)
import           Data.Text.Lazy               (fromStrict)


import           Control.Monad.Arguments
import           Control.Monad.Console
import           Control.Monad.FileSystem
import           Control.Monad.State
import           Control.Monad.Wait
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

newtype Resource config m a = Resource {
  unResource :: ReaderT config m a
} deriving(Functor, Applicative, Monad, MonadCatch, MonadThrow, MonadMask, MonadReader config, MonadTrans, MonadIO)

runResource :: config -> Resource config m a -> m a
runResource config f = unResource f `runReaderT` config

instance (MonadIO m, MonadThrow m, MonadWait m) => MonadWait (Resource c m) where
  wait = lift.wait

instance (MonadIO m, MonadThrow m, MonadArguments m) => MonadArguments (Resource c m) where
  arguments = lift arguments

instance (MonadIO m, MonadThrow m, MonadConsole m) => MonadConsole (Resource c m) where
  readStdin = lift readStdin
  writeStdout = lift.writeStdout
  writeStderr = lift.writeStderr

instance (MonadIO m, MonadThrow m, MonadFileSystem m) => MonadFileSystem (Resource c m) where
  readFile = lift . readFile
