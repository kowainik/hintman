{- | Functions to download files from GitHub by URLs. Usually useful when we
need to fetch content of the files accesible online.
-}

module Hintman.Download
       ( downloadFile
       ) where

import Network.HTTP.Client (Response (..), httpLbs)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))


-- | In-memory file download by URL sources by given URL.
downloadFile :: (MonadIO m, WithLog env m) => Text -> m (Maybe ByteString)
downloadFile url = do
    -- TODO: put in the context to not create each time
    man <- newTlsManager
    let req = fromString $ toString url
    log I $ "Attempting to download file from " <> url <> " ..."
    response <- liftIO $ httpLbs req man
    let status = statusCode $ responseStatus response
    let body = responseBody response
    case status of
        200 -> do
            log D $ "Successfully downloaded file from " <> url
            pure $ Just $ toStrict body
        _   -> do
            log E $ "Couldn't download file from " <> url
            pure Nothing
