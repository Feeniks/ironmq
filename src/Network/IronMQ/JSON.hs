{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}

module Network.IronMQ.JSON where 

import Network.IronMQ.Types

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

instance (FromJSON a, ToJSON a) => Message a where 
    toString = B.unpack . encode
    fromString = decode . B.pack

