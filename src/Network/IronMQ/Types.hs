{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Network.IronMQ.Types where 

import Control.Lens.TH
import Data.Typeable
import qualified Network.Wreq.Session as S

class Message a where 
	toString :: a -> String 
	fromString :: String -> Maybe a
	
type Host = String
type ProjectID = String
type Token = String
	
data QueueClient = QC {
	cHost :: Host,
	cProjectID :: ProjectID,
	cToken :: Token,
	cSession :: S.Session
} deriving Show

data QueueInfo = QueueInfo {
	_iSize :: Int
} deriving Show

data PushResponse = PushResponse {
	_rIDs :: [String]
} deriving Show

data QueueMessage b = QueueMessage {
	_mID :: String,
	_mTimeoutSecs :: Maybe Int,
	_mDelaySecs :: Maybe Int,
	_mExpirySecs :: Maybe Int,
	_mBody :: b
} deriving Show

data QueueMessages b = QMs {
	_msMessages :: [QueueMessage b]
}

data DeleteMessagesRequest = DeleteMessagesRequest {
	dmrIDs :: [String]
}

makeLenses ''QueueInfo
makeLenses ''PushResponse
makeLenses ''QueueMessage
makeLenses ''QueueMessages

data QueueException = 
	BadRequest 
	| Unauthorized 
	| QuotaExceeded 
	| NotFound 
	| ServiceUnavailable 
	| MessageParseError String 
	| Misc String 
	deriving (Show, Typeable)
