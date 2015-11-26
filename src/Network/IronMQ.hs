{-# LANGUAGE OverloadedStrings #-}

module Network.IronMQ(
    module Network.IronMQ.Types,
    createClient
) where

import Network.IronMQ.Types
import Network.IronMQ.Instances

import Control.Exception (Exception)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status

createClient :: MonadIO m => Host -> ProjectID -> Token -> m IronMQClient
createClient h p t = liftIO $ liftM (IronMQClient url authTok) (newManager tlsManagerSettings)
    where
        url = h ++ "/1/projects/" ++ p ++ "/"
        authTok = BS.append "OAuth " t

queueInfo :: (MonadIO m, MonadCatch m, MonadThrow m) => String -> IronMQ m QueueInfo
queueInfo qName = get ("queues/" ++ qName)

clearQueue :: (MonadIO m, MonadCatch m, MonadThrow m) => String -> IronMQ m ()
clearQueue qName = post ("queues/" ++ qName ++ "/clear") ("" :: String)

enqueue :: (MonadIO m, MonadCatch m, MonadThrow m, MessageBody b) => String -> [QueueMessage b] -> IronMQ m [String]
enqueue qName mx = post ("queues/" ++ qName ++ "/clear") (QMs mx)

dequeue :: (MonadIO m, MonadCatch m, MonadThrow m, MessageBody b) => String -> Int -> IronMQ m [QueueMessage b]
dequeue qName max = get ("queues/" ++ qName ++ "/messages")

get :: (MonadIO m, MonadCatch m, MonadThrow m, FromJSON r) => String -> IronMQ m r
get path = req =<< buildReq "GET" path (Nothing :: Maybe String)

post :: (MonadIO m, MonadCatch m, MonadThrow m, ToJSON b, FromJSON r) => String -> b -> IronMQ m r
post path body = req =<< buildReq "POST" path (Just body)

delete :: (MonadIO m, MonadCatch m, MonadThrow m, ToJSON b, FromJSON r) => String -> b -> IronMQ m r
delete path body = req =<< buildReq "DELETE" path (Just body)

buildReq :: (MonadThrow m, ToJSON b) => BS.ByteString -> String -> (Maybe b) -> IronMQ m Request
buildReq mthd path mbody = do
    auth <- viewR cToken
    initReq <- (url path >>= parseUrl)
    let bReq = initReq {
        method = mthd,
        requestHeaders = [("Content-Type", "application/json"), ("Authorization", auth)]
    }
    maybe (return bReq) (\body -> return $ bReq { requestBody = RequestBodyLBS (encode body) }) mbody

req :: (FromJSON r, MonadIO m, MonadCatch m) => Request -> IronMQ m r
req r = catchQ $ viewR cManager >>= httpLbs r >>= asJSON >>= return . responseBody

asJSON :: MonadIO m => FromJSON b => Response B.ByteString -> m (Response b)
asJSON resp = case (decode body) of
    Nothing -> liftIO . throwM . ParseError $ B.unpack body
    Just r -> return $ resp { responseBody = r }
    where body = responseBody resp

url :: (Functor m, Monad m) => String -> IronMQ m String
url s = fmap (++s) $ viewR cHost

viewR lens = fmap (view lens) ask

catchQ :: MonadCatch m => m a -> m a
catchQ a = catch a throwHTTP

throwHTTP :: MonadCatch m => HttpException -> m a
throwHTTP (StatusCodeException stat headers cookies)
    | code == 400 = throwM BadRequest
    | code == 401 = throwM Unauthorized
    | code == 403 = throwM QuotaExceeded
    | code == 404 = throwM NotFound
    | code == 503 = throwM ServiceUnavailable
    | otherwise = throwM (Misc $ show (StatusCodeException stat headers cookies))
    where code = statusCode stat
throwHTTP e = throwM (Misc $ show e)
