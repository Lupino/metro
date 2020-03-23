{-# LANGUAGE TypeFamilies #-}
module Metro.TP.Socket
  ( Socket
  , socket
  , mapTCPSocket
  , mapUDPSocket
  ) where

import           Data.List          (isPrefixOf)
import           Metro.Class        (Transport (..))
import           Metro.TP.TCPSocket
import           Metro.TP.UDPSocket

data Socket = TCP TCPSocket
    | UDP UDPSocket

instance Transport Socket where
  data TransportConfig Socket =
    TCPTC (TransportConfig TCPSocket)
    | UDPTC (TransportConfig UDPSocket)
  newTransport (TCPTC c) = TCP <$> newTransport c
  newTransport (UDPTC c) = UDP <$> newTransport c
  recvData (TCP tp) = recvData tp
  recvData (UDP tp) = recvData tp
  sendData (TCP tp) = sendData tp
  sendData (UDP tp) = sendData tp
  closeTransport (TCP tp) = closeTransport tp
  closeTransport (UDP tp) = closeTransport tp

socket :: String -> TransportConfig Socket
socket hostPort =
  if "udp" `isPrefixOf` hostPort then UDPTC $ udpSocket hostPort
                                 else TCPTC $ tcpSocket hostPort

mapTCPSocket :: TransportConfig TCPSocket -> TransportConfig Socket
mapTCPSocket = TCPTC

mapUDPSocket :: TransportConfig UDPSocket -> TransportConfig Socket
mapUDPSocket = UDPTC
