module CPI.Kubernetes.Action.Common(
    ignoringNotFound
) where

import           Control.Exception.Safe
import Servant.Common.Req
import Network.HTTP.Types.Status


ignoringNotFound :: (Monad m, MonadCatch m) => m b -> m (Maybe b)
ignoringNotFound f = do
  let catchNotFound :: (Monad m, MonadCatch m) => SomeException -> m (Maybe b)
      catchNotFound e = case fromException e of
        Just FailureResponse {
            responseStatus = Status {
              statusCode = 404
            }
          , responseContentType = _
          , responseBody = _
        } -> pure Nothing
        _ -> throwM e
  (Just <$> f) `catch` catchNotFound