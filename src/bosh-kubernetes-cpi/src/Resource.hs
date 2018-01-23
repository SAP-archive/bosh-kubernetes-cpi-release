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

import CPI.Base (CpiRunner(..), CpiConfiguration(..))
import           CPI.Base.System

import           Control.Exception.Safe
import           Control.Monad.Log
import           Control.Monad.Reader
import           Data.Text                    (Text)
import           Data.Text.Lazy               (fromStrict)

import           Control.Monad.State

import           Control.Effect.Class.Arguments
import           Control.Effect.Class.Console
import           Control.Effect.Class.FileSystem
import           Control.Effect.Class.Wait
import           Control.Effect
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

instance ( CpiConfiguration c m
         , Arguments m
         , FileSystem m) => CpiRunner c (Resource c m) m a where
  runCpi c r = runResource c r

newtype Resource config m a = Resource (ReaderT config m a)
  deriving(Functor, Applicative, Monad, MonadCatch, MonadThrow, MonadMask, MonadReader config, MonadIO)

instance MonadTrans (Resource c) where
  lift = Resource . lift

runResource :: (Monad m) => config -> Resource config m a -> m a
runResource config (Resource f) = f `runReaderT` config

instance (MonadIO m, MonadThrow m, Wait m) => Wait (Resource c m) where
  wait = lift.wait

instance (MonadIO m, MonadThrow m, Arguments m) => Arguments (Resource c m) where
  arguments = lift arguments

instance (MonadIO m, MonadThrow m, Console m) => Console (Resource c m) where
  readStdin = lift readStdin
  writeStdout = lift.writeStdout
  writeStderr = lift.writeStderr

instance (MonadIO m, MonadThrow m, FileSystem m) => FileSystem (Resource c m) where
  readFile = lift . readFile
