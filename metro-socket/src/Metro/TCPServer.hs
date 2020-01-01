{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.TCPServer
  ( TCPServer
  , tcpServer
  ) where

import           Control.Monad      (void)
import           Metro.Class        (Servable (..))
import           Metro.Socket       (listen)
import           Metro.TP.TCPSocket (TCPSocket, tcpSocket_)
import           Network.Socket     (Socket, SocketOption (KeepAlive), accept,
                                     setSocketOption)
import qualified Network.Socket     as Socket (close)
import           UnliftIO           (async, liftIO)

newtype TCPServer = TCPServer Socket

instance Servable TCPServer where
  data ServerConfig TCPServer = TCPConfig String
  type SID TCPServer = Socket
  type STP TCPServer = TCPSocket
  newServer (TCPConfig hostPort) = liftIO $ TCPServer <$> listen hostPort
  servOnce (TCPServer serv) done = do
    (sock, _) <- liftIO $ accept serv
    liftIO $ setSocketOption sock KeepAlive 1
    void $ async $ do
      done $ Just (sock, tcpSocket_ sock)
      liftIO $ Socket.close sock
  onConnEnter _ _ = return ()
  onConnLeave _ _ = return ()
  servClose (TCPServer serv) = liftIO $ Socket.close serv

tcpServer :: String -> ServerConfig TCPServer
tcpServer = TCPConfig
