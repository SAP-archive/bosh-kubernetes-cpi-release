{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CPI.Base(
  module CPI.Base.Messages,
  module CPI.Base.Errors,
  module CPI.Base.Inputs,
  module CPI.Base.Http,
  module CPI.Base.AgentConfig,
  ConfigParseException(..),
  RequestParseException(..),
  CPI(..),
  handleRequest,
  CpiMonad,
  run
) where

import  CPI.Base.Messages
import CPI.Base.Inputs
import CPI.Base.Http
import CPI.Base.AgentConfig
import           Control.Exception
import             Control.Monad.Catch
import           Data.Typeable
import           Data.Semigroup
import           Data.Text       (Text)
import           Data.Text       as Text
import           Control.Monad.Reader
import Control.Monad.Log

import qualified Data.Text.IO as Text.IO
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import           System.Environment           (getArgs)
import           System.IO

import CPI.Base.Errors

data ConfigParseException = ConfigParseException String
    deriving (Typeable, Show)

instance Exception ConfigParseException

newtype CpiMonad c a = CpiMonad {
  runCpi :: ReaderT c (LoggingT Text IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadReader c, MonadLog Text)

run :: (CPI c) => (Request -> CpiMonad c Response) -> IO ()
run handleRequest = do
  config <- loadConfig
            >>= parseConfig
  request <- readRequest
             >>= (either
              (throwM . RequestParseException)
              return) . eitherDecode'
  response <-
    runLoggingT (
      runReaderT (
        runCpi (
          handleRequest request
          ))
      config)
    (Text.IO.hPutStrLn stderr)
  BS.putStr $ Data.Aeson.encode response

loadConfig :: (MonadIO m) => m Text
loadConfig = do
  configFile:_ <- liftIO getArgs
  liftIO $ Text.IO.readFile configFile

readRequest :: (MonadIO m) => m ByteString
readRequest = liftIO BS.getContents

class CPI cpiConfig where
  parseConfig :: (MonadCatch m) => Text -> m cpiConfig
  createStemcell :: (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       FilePath
    -> StemcellProperties
    -> m StemcellId
  createVm ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       AgentId
    -> StemcellId
    -> VmProperties
    -> Networks
    -> DiskLocality
    -> Environment
    -> m VmId
  hasVm ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       VmId
    -> m Bool
  deleteVm ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       VmId
    -> m ()
  createDisk ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       Integer
    -> DiskProperties
    -> VmId
    -> m DiskId
  hasDisk ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       DiskId
    -> m Bool
  deleteDisk ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       DiskId
    -> m ()
  attachDisk ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       VmId
    -> DiskId
    -> m ()
  detachDisk ::
       (MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
       VmId
    -> DiskId
    -> m ()

handleRequest :: (CPI cpiConfig, MonadIO m, MonadCatch m, MonadReader cpiConfig m, MonadLog Text m) =>
                 Request
              -> m Response
handleRequest request@Request {
    method = methodName,
    arguments = args,
    context = context
  } = do
      logMessage $ "Receiving cpi request '" <> Text.pack (show request) <> "'"
      case (methodName, args) of
        ("create_stemcell", [filePath, cloudProperties]) -> do
          stemcellId <- join $ createStemcell
                            <$> parseArgument filePath
                            <*> parseArgument cloudProperties
          return $ createSuccess (Id stemcellId)
        ("create_vm", [agentId, stemcellId, cloudProperties, networkSpec, diskLocality, environment]) -> do
          vmCid <- join $ createVm
              <$> parseArgument agentId
              <*> parseArgument stemcellId
              <*> parseArgument cloudProperties
              <*> parseArgument networkSpec
              <*> parseArgument diskLocality
              <*> parseArgument environment
          return $ createSuccess (Id vmCid)
        ("has_vm", [vmId]) -> do
          hasVm <- join $ hasVm
                       <$> parseArgument vmId
          return $ createSuccess (Boolean hasVm)
        ("delete_vm", [vmId]) -> do
          join $ deleteVm
              <$> parseArgument vmId
          return $ createSuccess (Id "")
        ("create_disk", [size, diskProperties, vmId]) -> do
          diskCid <- join $ createDisk
              <$> parseArgument size
              <*> parseArgument diskProperties
              <*> parseArgument vmId
          return $ createSuccess (Id diskCid)
        ("has_disk", [diskId]) -> do
          hasDisk <- join $ hasDisk
                       <$> parseArgument diskId
          return $ createSuccess (Boolean hasDisk)
        ("delete_disk", [diskId]) -> do
          join $ deleteDisk
              <$> parseArgument diskId
          return $ createSuccess (Id "")
        ("attach_disk", [vmId, diskId]) -> do
          join $ attachDisk
              <$> parseArgument vmId
              <*> parseArgument diskId
          return $ createSuccess (Id "")
        ("detach_disk", [vmId, diskId]) -> do
          join $ detachDisk
              <$> parseArgument vmId
              <*> parseArgument diskId
          return $ createSuccess (Id "")
        _ -> return $ createFailureFromCpiError (NotImplemented (Text.concat ["Unknown message call '", methodName, "'"]))
