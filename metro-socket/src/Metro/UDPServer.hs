{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.UDPServer
  ( UDPServer
  , udpServer
  , getSocket
  ) where


import           Control.Monad      (void)
import           Data.IOMap         (IOMap)
import qualified Data.IOMap         as Map (delete, empty, insert, lookup)
import           Metro.Class        (Servable (..), TransportConfig)
import           Metro.Socket       (bindTo)
import           Metro.TP.BS        (BSHandle, bsTPConfig, closeBSHandle, feed,
                                     newBSHandle)
import           Metro.TP.UDPSocket (UDPSocket, doSendAll, recvFrom, udpSocket_)
import           Network.Socket     (SockAddr, Socket)
import qualified Network.Socket     as Socket (close)
import           UnliftIO

data UDPServer = UDPServer Socket (IOMap SockAddr BSHandle)

instance Servable UDPServer where
  data ServerConfig UDPServer = UDPConfig String
  type SID UDPServer = SockAddr
  type STP UDPServer = UDPSocket
  newServer (UDPConfig hostPort) = do
    sock <- liftIO $ bindTo hostPort
    UDPServer sock <$> Map.empty

  servOnce us@(UDPServer serv handleList) done = do
    (bs, addr) <- liftIO $ recvFrom serv 65535

    bsHandle <- Map.lookup addr handleList
    case bsHandle of
      Just h  -> feed h bs
      Nothing ->
        void . async $ do
          h <- newBSHandle bs
          config <- newTransportConfig us addr h
          done $ Just (addr, config)
          closeBSHandle h
          Map.delete addr handleList

  onConnEnter _ _ = return ()
  onConnLeave (UDPServer _ handleList) addr = Map.delete addr handleList
  servClose (UDPServer serv _) = liftIO (Socket.close serv)

udpServer :: String -> ServerConfig UDPServer
udpServer = UDPConfig

newTransportConfig
  :: (MonadIO m)
  => UDPServer
  -> SockAddr
  -> BSHandle
  -> m (TransportConfig UDPSocket)
newTransportConfig (UDPServer sock handleList) addr h = do
  Map.insert addr h handleList
  return $ udpSocket_ $ bsTPConfig h (doSendAll sock addr) $ show addr

getSocket :: UDPServer -> Socket
getSocket (UDPServer sock _) = sock
