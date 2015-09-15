{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Network.IronMQ.Instances where 

import Network.IronMQ.Types

import Control.Applicative
import Control.Exception (Exception)
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)

instance FromJSON QueueInfo where 
	parseJSON (Object v) = QueueInfo <$> v .: "size"
	parseJSON _ = mzero
	
instance FromJSON PushResponse where 
	parseJSON (Object v) = PushResponse <$> v .: "ids"
	parseJSON _ = mzero
	
instance Message b => FromJSON (QueueMessage b) where 
	parseJSON (Object v) = do 
		id <- v .: "id"
		mbody <- fmap fromString $ v .: "body"
		timeout <- v .:? "timeout"
		maybe mzero (return . QueueMessage id timeout Nothing Nothing) mbody
	parseJSON _ = mzero
	
instance Message b => ToJSON (QueueMessage b) where 
	toJSON m = 
		object [
			"body" .= (toString $ m ^. mBody), 
			"timeout" .= (fromMaybe 60 $ m ^. mTimeoutSecs),
			"delay" .= (fromMaybe 0 $ m ^. mDelaySecs),
			"expires_in" .= (fromMaybe 604800 $ m ^. mExpirySecs)
		]
		
instance Message b => FromJSON (QueueMessages b) where 
	parseJSON (Object v) = QMs <$> v .: "messages"
	parseJSON _ = mzero

instance Message b => ToJSON (QueueMessages b) where 
	toJSON qms = object ["messages" .= (qms ^. msMessages)]
	
instance ToJSON DeleteMessagesRequest where 
	toJSON req = object ["ids" .= dmrIDs req]
	
instance Exception QueueException