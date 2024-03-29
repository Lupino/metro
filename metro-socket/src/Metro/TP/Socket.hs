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
  newTP (TCPTC c) = TCP <$> newTP c
  newTP (UDPTC c) = UDP <$> newTP c
  recvData (TCP tp) = recvData tp
  recvData (UDP tp) = recvData tp
  sendData (TCP tp) = sendData tp
  sendData (UDP tp) = sendData tp
  closeTP (TCP tp) = closeTP tp
  closeTP (UDP tp) = closeTP tp
  getTPName (TCP tp) = getTPName tp
  getTPName (UDP tp) = getTPName tp

socket :: String -> TransportConfig Socket
socket hostPort =
  if "udp" `isPrefixOf` hostPort then UDPTC $ udpSocket hostPort
                                 else TCPTC $ tcpSocket hostPort

mapTCPSocket :: TransportConfig TCPSocket -> TransportConfig Socket
mapTCPSocket = TCPTC

mapUDPSocket :: TransportConfig UDPSocket -> TransportConfig Socket
mapUDPSocket = UDPTC
