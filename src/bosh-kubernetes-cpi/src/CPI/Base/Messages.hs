{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CPI.Base.Messages where

import           Data.Aeson
import           Data.Semigroup
import           GHC.Generics

import           Control.Exception
import           Control.Monad.Catch
import           Data.Map.Strict     (Map)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Typeable

data Request = Request {
  method    :: Text,
  arguments :: [Value],
  context   :: Map Text Text
} deriving (Generic, Show)

instance FromJSON Request where

data ResultType = Id Text | Boolean Bool deriving (Show)

instance ToJSON ResultType where
    -- this generates a Value
    toJSON (Id text) = String text
    toJSON (Boolean text) = Bool text

    -- this encodes directly to a bytestring Builder
    -- toEncoding (CPIError errorType errorMessage okToRetry) =
    --     pairs ("type" .= errorType <> "message" .= errorMessage <> "ok_to_retry" .= okToRetry)

data Response = Response {
  result :: Maybe ResultType,
  error  :: Maybe CPIError,
  log    :: Text
} deriving (Show, Generic)

instance ToJSON Response where

createSuccess :: ResultType -> Response
createSuccess msg = Response (Just msg) Nothing ""

createFailureFromCpiError :: ToCpiError e => e -> Response
createFailureFromCpiError e = Response Nothing (Just $ toCpiError e) ""

data RequestParseException = RequestParseException String
    deriving (Typeable, Show)

instance Exception RequestParseException

parseArgument :: (MonadThrow m, FromJSON a) => Value -> m a
parseArgument input = case fromJSON input of
  Success a -> return a
  Error msg -> throwM (RequestParseException msg)

class ToCpiError a where
  toCpiError :: a -> CPIError

instance ToCpiError Text where
  toCpiError msg = CPIError {
    errorType = "UnexpectedException",
    errorMessage = msg,
    okToRetry = False
  }

instance ToCpiError String where
  toCpiError msg = CPIError {
    errorType = "UnexpectedException",
    errorMessage = Text.pack msg,
    okToRetry = False
  }

data CPIError = CPIError {
  errorType    :: Text,
  errorMessage :: Text,
  okToRetry    :: Bool
} deriving (Show)

instance ToJSON CPIError where
    -- this generates a Value
    toJSON (CPIError errorType errorMessage okToRetry) =
        object ["type" .= errorType, "message" .= errorMessage, "ok_to_retry" .= okToRetry]

    -- this encodes directly to a bytestring Builder
    toEncoding (CPIError errorType errorMessage okToRetry) =
        pairs ("type" .= errorType <> "message" .= errorMessage <> "ok_to_retry" .= okToRetry)
