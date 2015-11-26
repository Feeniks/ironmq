{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Network.IronMQ.Types where

import Control.Lens.TH
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Typeable
import Network.HTTP.Conduit (Manager)

class MessageBody a where
    serialize :: a -> B.ByteString
    deserialize :: B.ByteString -> Maybe a

type Host = String
type ProjectID = String
type Token = BS.ByteString

data IronMQClient = IronMQClient {
    _cHost :: Host,
    _cToken :: Token,
    _cManager :: Manager
}

type IronMQ = ReaderT IronMQClient

data QueueMessage b = QueueMessage {
    _mID :: String,
    _mTimeoutSecs :: Maybe Int,
    _mDelaySecs :: Maybe Int,
    _mExpirySecs :: Maybe Int,
    _mBody :: b
} deriving Show

newtype QueueMessages b = QMs [QueueMessage b]

data QueueInfo = QueueInfo Int deriving Show

data PushResponse = PushResponse [String] deriving Show

data DeleteMessagesRequest = DeleteMessagesRequest [String] deriving Show

data QueueException =
    BadRequest
    | Unauthorized
    | QuotaExceeded
    | NotFound
    | ServiceUnavailable
    | ParseError String
    | Misc String
    deriving (Show, Typeable)

makeLenses ''IronMQClient
makeLenses ''QueueMessage
