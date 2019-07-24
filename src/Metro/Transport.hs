{-# LANGUAGE TypeFamilies #-}
module Metro.Transport
  ( Transport (..)
  , TransportError (..)
  ) where

import           Control.Exception (Exception)
import           Data.ByteString   (ByteString)

class Transport transport where
  data TransportConfig transport
  newTransport   :: TransportConfig transport -> IO transport
  recvData       :: transport -> Int -> IO ByteString
  sendData       :: transport -> ByteString -> IO ()
  closeTransport :: transport -> IO ()

data TransportError = TransportClosed
  deriving (Show, Eq, Ord)

instance Exception TransportError
