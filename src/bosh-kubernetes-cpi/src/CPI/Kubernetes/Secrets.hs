{-# LANGUAGE RankNTypes #-}
module CPI.Kubernetes.Secrets(
  modifySettings
) where

import           CPI.Base               as Base

import           Control.Monad
import           Control.Monad.Catch
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy   as LBS
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Debug.Trace

withBase64 :: (Monad m, MonadThrow m) => (ByteString -> m ByteString) -> Text -> m Text
withBase64 modify base64 = do
  return $ trace ("encoded: " <> show base64) id
  modified <- modify $ tap "decoded" $ decode base64
  return $ encode $ tap "modified" $ modified
  where
    decode :: Text -> BS.ByteString
    decode = Base64.decodeLenient . Text.encodeUtf8
    encode :: BS.ByteString -> Text
    encode = Text.decodeUtf8 . Base64.encode

modifySettings :: (MonadThrow m, ToJSON v, FromJSON v) => (v -> v) -> Text -> m Text
modifySettings modify = withBase64 $ \bs ->
  case eitherDecode $ LBS.fromStrict bs of
    Left msg   -> throwM $ Base.CloudError $ Text.pack msg
    Right json -> return $ LBS.toStrict $ encode $ modify json

tap :: (Show a) => String -> a -> a
tap prefix a =
  trace (prefix <> ": " <> show a) a
