{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.TCP
  ( TCPServer
  , tcpConfig
  ) where

import           Metro.Class            (Servable (..))
import           Metro.Socket           (listen)
import qualified Metro.Transport.Socket as T (Socket, rawSocket)
import           Network.Socket         (Socket, SocketOption (KeepAlive),
                                         accept, setSocketOption)
import qualified Network.Socket         as Socket (close)
import           UnliftIO               (liftIO)

newtype TCPServer = TCPServer Socket

instance Servable TCPServer where
  data ServerConfig TCPServer = TCPConfig String
  type SID TCPServer = Socket
  type STP TCPServer = T.Socket
  newServer (TCPConfig hostPort) = liftIO $ TCPServer <$> listen hostPort
  servOnce (TCPServer serv) = do
    (sock, _) <- liftIO $ accept serv
    liftIO $ setSocketOption sock KeepAlive 1
    return$ Just (sock, T.rawSocket sock)
  onConnEnter _ _ = return ()
  onConnLeave _ _ = return ()
  servClose (TCPServer serv) = liftIO $ Socket.close serv

tcpConfig :: String -> ServerConfig TCPServer
tcpConfig = TCPConfig
