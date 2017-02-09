{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module CPI.Base.Errors where

import           Control.Exception
import           CPI.Base.Messages
import           Data.Text         (Text)
import           Data.Typeable

data CpiError = forall e . Exception e => CpiError Text e
  deriving Typeable

instance Show CpiError where
  show (CpiError msg e) = show e ++ show msg

instance Exception CpiError

cpiErrorToException :: Exception e => Text -> e -> SomeException
cpiErrorToException msg = toException . CpiError msg

cpiErrorFromException :: Exception e => SomeException -> Maybe e
cpiErrorFromException x = do
    CpiError _ a <- fromException x
    cast a

data NotImplemented = NotImplemented Text
  deriving (Typeable, Show)

instance Exception NotImplemented where
    toException e@(NotImplemented msg)  = cpiErrorToException msg e
    fromException = cpiErrorFromException

instance ToCpiError NotImplemented where
  toCpiError (NotImplemented msg) = CPIError {
    errorType = "Bosh::Clouds::NotImplemented",
    errorMessage = msg,
    okToRetry = False
  }

data NotSupported = NotSupported Text
  deriving (Typeable, Show)

instance Exception NotSupported where
    toException  e@(NotSupported msg) = cpiErrorToException msg e
    fromException = cpiErrorFromException

instance ToCpiError NotSupported where
  toCpiError (NotSupported msg) = CPIError {
    errorType = "Bosh::Clouds::NotSupported",
    errorMessage = msg,
    okToRetry = False
  }

data BaseCloudError = forall e . Exception e => BaseCloudError Text e
  deriving Typeable

instance Show BaseCloudError where
  show (BaseCloudError msg e) = show e ++ show msg

instance Exception BaseCloudError

baseCloudErrorToException :: Exception e => Text -> e -> SomeException
baseCloudErrorToException msg = toException . BaseCloudError msg

baseCloudErrorFromException :: Exception e => SomeException -> Maybe e
baseCloudErrorFromException x = do
    BaseCloudError _ a <- fromException x
    cast a

data CloudError = CloudError Text
  deriving (Typeable, Show)

instance Exception CloudError where
    toException e@(CloudError msg)  = baseCloudErrorToException msg e
    fromException = baseCloudErrorFromException

instance ToCpiError CloudError where
  toCpiError (CloudError msg) = CPIError {
    errorType = "Bosh::Clouds::CloudError",
    errorMessage = msg,
    okToRetry = False
  }

data VMNotFound = VMNotFound Text
  deriving (Typeable, Show)

instance Exception VMNotFound where
    toException  e@(VMNotFound msg) = baseCloudErrorToException msg e
    fromException = baseCloudErrorFromException

instance ToCpiError VMNotFound where
  toCpiError (VMNotFound msg) = CPIError {
    errorType = "Bosh::Clouds::VMNotFound",
    errorMessage = msg,
    okToRetry = False
  }

data RetriableBaseCloudError = forall e . Exception e => RetriableBaseCloudError Text Bool e
  deriving Typeable

instance Show RetriableBaseCloudError where
  show (RetriableBaseCloudError msg retry e) = show e ++ show msg ++ show retry

instance Exception RetriableBaseCloudError

retryableBaseCloudErrorToException :: Exception e => Text -> Bool -> e -> SomeException
retryableBaseCloudErrorToException msg retry = toException . RetriableBaseCloudError msg retry

retryableBaseCloudErrorFromException :: Exception e => SomeException -> Maybe e
retryableBaseCloudErrorFromException x = do
    RetriableBaseCloudError _ _ a <- fromException x
    cast a

data NoDiskSpace = NoDiskSpace Text Bool
  deriving (Typeable, Show)

instance Exception NoDiskSpace where
    toException  e@(NoDiskSpace msg retry) = retryableBaseCloudErrorToException msg retry e
    fromException = retryableBaseCloudErrorFromException

instance ToCpiError NoDiskSpace where
  toCpiError (NoDiskSpace msg retry) = CPIError {
    errorType = "Bosh::Clouds::NoDiskSpace",
    errorMessage = msg,
    okToRetry = retry
  }

data DiskNotAttached = DiskNotAttached Text Bool
  deriving (Typeable, Show)

instance Exception DiskNotAttached where
    toException  e@(DiskNotAttached msg retry) = retryableBaseCloudErrorToException msg retry e
    fromException = retryableBaseCloudErrorFromException

instance ToCpiError DiskNotAttached where
  toCpiError (DiskNotAttached msg retry) = CPIError {
    errorType = "Bosh::Clouds::DiskNotAttached",
    errorMessage = msg,
    okToRetry = retry
  }

data DiskNotFound = DiskNotFound Text Bool
  deriving (Typeable, Show)

instance Exception DiskNotFound where
    toException  e@(DiskNotFound msg retry) = retryableBaseCloudErrorToException msg retry e
    fromException = retryableBaseCloudErrorFromException

instance ToCpiError DiskNotFound where
  toCpiError (DiskNotFound msg retry) = CPIError {
    errorType = "Bosh::Clouds::DiskNotFound",
    errorMessage = msg,
    okToRetry = retry
  }

data VMCreationFailed = VMCreationFailed Text Bool
  deriving (Typeable, Show)

instance Exception VMCreationFailed where
    toException  e@(VMCreationFailed msg retry) = retryableBaseCloudErrorToException msg retry e
    fromException = retryableBaseCloudErrorFromException

instance ToCpiError VMCreationFailed where
  toCpiError (VMCreationFailed msg retry) = CPIError {
    errorType = "Bosh::Clouds::VMCreationFailed",
    errorMessage = msg,
    okToRetry = retry
  }
