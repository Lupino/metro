{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.TCP
  ( TCPServer
  , tcpConfig
  ) where

import           Metro.Servable         (Servable (..))
import           Metro.Socket           (listen)
import qualified Metro.Transport.Socket as T (Socket, rawSocket)
import           Network.Socket         (Socket, SocketOption (KeepAlive),
                                         accept, setSocketOption)
import qualified Network.Socket         as Socket (close)
import           UnliftIO               (liftIO)

newtype TCPServer = TCPServer Socket

instance Servable TCPServer where
  data ServConfig TCPServer = TCPConfig String
  type ServID TCPServer = Socket
  type STP TCPServer = T.Socket
  newServ (TCPConfig hostPort) = liftIO $ TCPServer <$> listen hostPort
  servOnce (TCPServer serv) = do
    (sock, _) <- liftIO $ accept serv
    liftIO $ setSocketOption sock KeepAlive 1
    return$ Just (sock, T.rawSocket sock)
  onConnEnter _ _ = return ()
  onConnLeave _ _ = return ()
  close (TCPServer serv) = liftIO $ Socket.close serv

tcpConfig :: String -> ServConfig TCPServer
tcpConfig = TCPConfig
