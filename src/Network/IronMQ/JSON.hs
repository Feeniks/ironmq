{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}

module Network.IronMQ.JSON where

import Network.IronMQ.Types

import Data.Aeson

instance (FromJSON b, ToJSON b) => MessageBody b where
    serialize = encode
    deserialize = decode
