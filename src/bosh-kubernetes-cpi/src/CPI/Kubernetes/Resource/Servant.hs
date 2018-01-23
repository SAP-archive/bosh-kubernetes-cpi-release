{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module CPI.Kubernetes.Resource.Servant(
    restCall
  , restCallGetter
  , RestCall
) where

import qualified CPI.Base                       as Base
import CPI.Kubernetes.Config

import           Control.Lens
import           Control.Monad.Catch
import           Data.Maybe
import           Data.ByteString
import           Data.Semigroup
import Data.Aeson
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Log
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text.Encoding
import qualified Data.Text.IO                   as Text.IO
import Data.ByteString.Lazy (toStrict)
import qualified Servant.Common.BaseUrl         as Url
import qualified Servant.Common.Req             as Servant
import qualified Network.HTTP.Types.Status as Http
import           System.IO
import Data.IORef
import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (Header, hAuthorization)
import           Network.TLS hiding (Credentials, Header)
import           Network.TLS.Extra.Cipher
import           Data.Default.Class
import           Data.X509
import CPI.Base.System
import Control.Effect.Class.Console
import Control.Effect.Class.FileSystem

tlsSettings :: Url.BaseUrl -> Credential -> TLSSettings
tlsSettings serverUrl clientCredentials =
  let
    baseParams = clientParams serverUrl
    -- TODO should we not hand out our creds for any particular input?
    onCertificateRequest :: ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName]) -> IO (Maybe (CertificateChain, PrivKey))
    onCertificateRequest x = pure $ Just clientCredentials

    -- TODO we should actually check for a valid cert according to our custom ca cert.
    onServerCertificate _ _ _ _ = pure []
    in
      TLSSettings (baseParams {clientHooks=def {onCertificateRequest=onCertificateRequest, onServerCertificate=onServerCertificate}})

clientParams :: Url.BaseUrl -> ClientParams
clientParams serverUrl =
  ClientParams {
    clientUseMaxFragmentLength = def,
    clientServerIdentification = (Url.baseUrlHost serverUrl, undefined),
    clientUseServerNameIndication = False,
    clientWantSessionResume = def,
    clientShared = def,
    clientHooks = def,
    clientSupported = def {supportedCiphers=ciphersuite_all},
    clientDebug = def
  }

type RestCall model =
        Manager
     -> Url.BaseUrl
     -> Servant.ClientM model

restCall :: (MonadIO m, MonadThrow m, HasConfig c, MonadReader c m, MonadLog (WithSeverity Text) m, Console m, FileSystem m) =>
            RestCall model
         -> m model
restCall f = do
  cluster <- asks asConfig
  let baseUrl = cluster & clusterAccess & server
  creds <- cluster & clusterAccess & credentials
  manager <- liftIO $ newManager $ createManagerSettings baseUrl creds
  result <- liftIO $ runExceptT $ f manager baseUrl
  either throwM return result

restCallGetter :: (MonadIO m, MonadCatch m, HasConfig c, MonadReader c m, MonadLog (WithSeverity Text) m, Console m, FileSystem m) =>
             RestCall model
          -> m (Maybe model)
restCallGetter f = do
  result <- try(restCall f)
  return $ case result of
    Right model -> Just model
    Left Servant.FailureResponse {
      Servant.responseStatus = Http.Status {
        Http.statusCode = 404
      },
      Servant.responseContentType = _,
      Servant.responseBody = _
    } -> Nothing
    Left servantError -> throwM servantError

createManagerSettings :: Url.BaseUrl -> Credentials -> ManagerSettings
createManagerSettings baseUrl (ClientCertificate clientCert) =
  let settings = tlsSettings baseUrl clientCert
  in
  (mkManagerSettings settings Nothing)
    {
        managerModifyResponse = logResponse
      , managerModifyRequest = logRequest
    }
createManagerSettings baseUrl (Token token) =
  let settings = def {
    settingDisableCertificateValidation = True
  }
  in
  (mkManagerSettings settings Nothing)
    {
        managerModifyResponse = logResponse
      , managerModifyRequest = logRequest <$> addHeader (hAuthorization, "Bearer " <> Text.Encoding.encodeUtf8 token)
    }


addHeader :: Header -> Request -> Request
addHeader header request = request {
  requestHeaders = header : requestHeaders request
}

logRequest req = do
  Text.IO.hPutStrLn stderr (Text.pack (show req))
  pure req

logResponse :: Response BodyReader -> IO (Response BodyReader)
logResponse res = do
  let constBodyReader :: [ByteString] -> IO BodyReader
      constBodyReader input = do
        iinput <- newIORef input
        return $ atomicModifyIORef iinput $ \input' ->
              case input' of
                  [] -> ([], empty)
                  x:xs -> (xs, x)
  body <- brConsume $ responseBody res
  response <- showResponse res body
  Text.IO.hPutStrLn stderr response
  reader <- constBodyReader body
  return res {
    responseBody = reader
  }

showResponse :: Response BodyReader -> [ByteString] -> IO Text
showResponse res body =
  return $ Text.unlines
    [
      "Response {"
    , "  responseStatus     = " <> Text.pack (show (responseStatus res))
    , "  responseVersion    = " <> Text.pack (show (responseVersion res))
    , "  responseHeaders    = " <> Text.pack (show (responseHeaders res))
    , "  responseBody       = " <> Text.Encoding.decodeUtf8 (Data.ByteString.concat body)
    , "}"
    ]
