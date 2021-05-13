{-# LANGUAGE TypeFamilies #-}
module Metro.TP.TCPSocket
  ( TCPSocket
  , tcpSocket
  , tcpSocket_
  ) where

import           Metro.Class               (Transport (..))
import           Metro.Socket              (connect)
import           Network.Socket            (Socket, close, getPeerName)
import           Network.Socket.ByteString (recv, sendAll)

newtype TCPSocket = TCPSocket Socket

instance Transport TCPSocket where
  data TransportConfig TCPSocket =
      RawSocket Socket
    | SocketUri String
  newTP (RawSocket soc) = pure $ TCPSocket soc
  newTP (SocketUri h)   = TCPSocket <$> connect h
  recvData (TCPSocket soc) = recv soc
  sendData (TCPSocket soc) = sendAll soc
  closeTP (TCPSocket soc) = close soc
  getTPName (TCPSocket soc) = show <$> getPeerName soc

tcpSocket_ :: Socket -> TransportConfig TCPSocket
tcpSocket_ = RawSocket

tcpSocket :: String -> TransportConfig TCPSocket
tcpSocket = SocketUri
