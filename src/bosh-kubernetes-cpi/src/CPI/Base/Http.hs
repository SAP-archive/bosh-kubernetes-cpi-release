{-# LANGUAGE OverloadedStrings #-}
module CPI.Base.Http(
  tlsSettings
) where

import           Data.Text                      (Text)
import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.TLS
import           Network.TLS.Extra.Cipher
import           Data.Default.Class
import           Data.X509
import qualified Servant.Common.BaseUrl         as Url

tlsSettings :: Url.BaseUrl -> Credential -> TLSSettings
tlsSettings serverUrl clientCredentials =
  let
    baseParams = clientParams serverUrl
    -- TODO should we not hand out our creds for any particular input?
    onCertificateRequest :: ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName]) -> IO (Maybe (CertificateChain, PrivKey))
    onCertificateRequest x = do
      return $ Just clientCredentials

    -- TODO we should actually check for a valid cert according to our custom ca cert.
    onServerCertificate _ _ _ _ = do
      return []
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
