{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Network.IronMQ.Instances where

import Network.IronMQ.Types

import Control.Applicative
import Control.Exception (Exception)
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default.Class
import Data.Maybe (fromMaybe)

instance MessageBody B.ByteString where
    serialize = id
    deserialize = Just . id

instance Default b => Default (QueueMessage b) where
    def =
        QueueMessage {
            _mID = "",
            _mTimeoutSecs = Nothing,
            _mDelaySecs = Nothing,
            _mExpirySecs = Nothing,
            _mBody = def
        }

instance FromJSON QueueInfo where
    parseJSON (Object v) = QueueInfo <$> v .: "size"
    parseJSON _ = mzero

instance FromJSON PushResponse where
    parseJSON (Object v) = PushResponse <$> v .: "ids"
    parseJSON _ = mzero

instance MessageBody b => FromJSON (QueueMessages b) where
    parseJSON (Object v) = QMs <$> v .: "messages"
    parseJSON _ = mzero

instance MessageBody b => FromJSON (QueueMessage b) where
    parseJSON (Object v) = do
        mid <- v .: "id"
        mbody <- fmap (deserialize . B.pack) (v .: "body")
        timeout <- v .:? "timeout"
        maybe mzero (return . QueueMessage mid timeout Nothing Nothing) mbody
    parseJSON _ = mzero

instance MessageBody b => ToJSON (QueueMessage b) where
    toJSON m =
        object [
            "body" .= (B.unpack . serialize $ m ^. mBody),
            "timeout" .= (fromMaybe 60 $ m ^. mTimeoutSecs),
            "delay" .= (fromMaybe 0 $ m ^. mDelaySecs),
            "expires_in" .= (fromMaybe 604800 $ m ^. mExpirySecs)
        ]

instance MessageBody b => ToJSON (QueueMessages b) where
    toJSON (QMs mx) = object ["messages" .= mx]

instance ToJSON DeleteMessagesRequest where
    toJSON (DeleteMessagesRequest ids) = object ["ids" .= ids]

instance Exception QueueException
