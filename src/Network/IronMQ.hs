{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Network.IronMQ(
	Network.IronMQ.Types.Host,
	Network.IronMQ.Types.ProjectID,
	Network.IronMQ.Types.Token,
	Network.IronMQ.Types.QueueClient,
	Network.IronMQ.Types.Message,
	Network.IronMQ.Types.QueueMessage(..),
	createClient,
	queueInfo,
	queueClear,
	queuePush,
	queueFetch,
	queueDelete
) where 

import Network.IronMQ.Types
import Network.IronMQ.Instances

import Control.Applicative
import Control.Exception (Exception, catch, throw, throwIO)
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Client (HttpException(..))
import qualified Network.HTTP.Types.Status as NS
import Network.Wreq
import qualified Network.Wreq.Session as S

data QueueException = 
	BadRequest 
	| Unauthorized 
	| QuotaExceeded 
	| NotFound 
	| ServiceUnavailable 
	| MessageParseError String 
	| Misc String 
	deriving (Show, Typeable)

instance Exception QueueException

throwHTTP :: HttpException -> IO a
throwHTTP (StatusCodeException stat headers cookies)
	| code == 400 = throwIO BadRequest
	| code == 401 = throwIO Unauthorized
	| code == 403 = throwIO QuotaExceeded
	| code == 404 = throwIO NotFound
	| code == 503 = throwIO ServiceUnavailable
	| otherwise = throwIO (Misc $ show (StatusCodeException stat headers cookies))
	where code = NS.statusCode stat
throwHTTP e = throwIO (Misc $ show e)

throwJSON :: JSONError -> IO a
throwJSON e = throwIO (MessageParseError $ show e)

createClient :: Host -> ProjectID -> Token -> IO QueueClient
createClient h p t = S.withSession $ \s -> return (QC (h ++ "/1/projects/" ++ p) p t s)

catchQ :: IO a -> IO a
catchQ a = catch (catch a throwHTTP) throwJSON

_options t = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [B.pack $ "OAuth " ++ t]

queueInfo :: QueueClient -> String -> IO QueueInfo 
queueInfo (QC h p t s) name = catchQ $ do 
	let url = h ++ "/queues/" ++ name
	rsp <- S.getWith (_options t) s url >>= asJSON
	return $ rsp ^. responseBody
	
queueClear :: QueueClient -> String -> IO ()
queueClear (QC h p t s) name = catchQ $ do 
	let url = h ++ "/queues/" ++ name ++ "/clear"
	S.postWith (_options t) s url ("" :: LB.ByteString)
	return ()
	
queuePush :: Message b => QueueClient -> String -> [QueueMessage b] -> IO [String]
queuePush (QC h p t s) name messages = catchQ $ do 
	let url = h ++ "/queues/" ++ name ++ "/messages"
	let payload = encode $ QMs messages
	rsp <- S.postWith (_options t) s url payload >>= asJSON
	return $ rsp ^. (responseBody . rIDs)
	
queueFetch :: Message b => QueueClient -> String -> Int -> Bool -> IO [QueueMessage b]
queueFetch (QC h p t s) name max delete = catchQ $ do 
	let url = h ++ "/queues/" ++ name ++ "/messages"
	let opts = (_options t) & param "n" .~ [T.pack $ show max] & param "delete" .~ [T.pack $ fmap toLower $ show delete]
	rsp <- S.getWith opts s url >>= asJSON
	return $ rsp ^. (responseBody . msMessages)
	
queueDelete :: QueueClient -> String -> String -> IO ()
queueDelete (QC h p t s) name id = catchQ $ do
	let url = h ++ "/queues/" ++ name ++ "/messages/" ++ id
	S.deleteWith (_options t) s url
	return ()
