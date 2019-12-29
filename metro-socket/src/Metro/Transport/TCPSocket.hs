{-# LANGUAGE TypeFamilies #-}
module Metro.Transport.TCPSocket
  ( TCPSocket
  , tcpSocket
  , tcpSocket_
  ) where

import           Metro.Class               (Transport (..))
import           Metro.Socket              (connect)
import           Network.Socket            (Socket, close)
import           Network.Socket.ByteString (recv, sendAll)

newtype TCPSocket = TCPSocket Socket

instance Transport TCPSocket where
  data TransportConfig TCPSocket =
      RawSocket Socket
    | SocketUri String
  newTransport (RawSocket soc) = pure $ TCPSocket soc
  newTransport (SocketUri h)   = TCPSocket <$> connect h
  recvData (TCPSocket soc) = recv soc
  sendData (TCPSocket soc) = sendAll soc
  closeTransport (TCPSocket soc) = close soc

tcpSocket_ :: Socket -> TransportConfig TCPSocket
tcpSocket_ = RawSocket

tcpSocket :: String -> TransportConfig TCPSocket
tcpSocket = SocketUri
