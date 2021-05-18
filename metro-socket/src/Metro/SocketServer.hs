{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.SocketServer
  ( SocketServer (..)
  , socketServer
  , SSSID (..)
  ) where

import           Data.List       (isPrefixOf)
import           Metro.Class     (Servable (..), Transport (..),
                                  TransportConfig)
import           Metro.TCPServer
import qualified Metro.TP.Socket as T (Socket, mapTCPSocket, mapUDPSocket)
import           Metro.UDPServer
import           Network.Socket  (SockAddr, Socket)

data SocketServer = TCP TCPServer
    | UDP UDPServer

data SSSID = TCPSID Socket
    | UDPSID SockAddr

instance Servable SocketServer where
  data ServerConfig SocketServer = SSConfig String
  type SID SocketServer = SSSID
  type STP SocketServer = T.Socket
  newServer (SSConfig hostPort) =
    if "udp" `isPrefixOf` hostPort then UDP <$> newServer (udpServer hostPort)
    else TCP <$> newServer (tcpServer hostPort)
  servOnce (TCP s) done = servOnce s $ mapTCPServOnceDone done
  servOnce (UDP s) done = servOnce s $ mapUDPServOnceDone done
  onConnEnter (TCP s) (TCPSID sid) = onConnEnter s sid
  onConnEnter (UDP s) (UDPSID sid) = onConnEnter s sid
  onConnEnter _ _                  = error "onConnEnter invalid type"
  onConnLeave (TCP s) (TCPSID sid) = onConnLeave s sid
  onConnLeave (UDP s) (UDPSID sid) = onConnLeave s sid
  onConnLeave _ _                  = error "onConnLeave invalid type"
  servClose (TCP s) = servClose s
  servClose (UDP s) = servClose s

mapTCPServOnceDone
  :: (Maybe (SID SocketServer, TransportConfig (STP SocketServer)) -> m ())
  -> Maybe (SID TCPServer, TransportConfig (STP TCPServer)) -> m ()
mapTCPServOnceDone done Nothing = done Nothing
mapTCPServOnceDone done (Just (sid, stp)) = done (Just (TCPSID sid, T.mapTCPSocket stp))

mapUDPServOnceDone
  :: (Maybe (SID SocketServer, TransportConfig (STP SocketServer)) -> m ())
  -> Maybe (SID UDPServer, TransportConfig (STP UDPServer)) -> m ()
mapUDPServOnceDone done Nothing = done Nothing
mapUDPServOnceDone done (Just (sid, stp)) = done (Just (UDPSID sid, T.mapUDPSocket stp))

socketServer :: String -> ServerConfig SocketServer
socketServer = SSConfig
