{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.UDPServer
  ( UDPServer
  , udpServer
  ) where

import           Control.Monad             (void)
import           Data.IOHashMap            (IOHashMap)
import qualified Data.IOHashMap            as HM (delete, empty, insert, lookup)
import           Metro.Class               (Servable (..), TransportConfig)
import           Metro.Socket              (bindTo)
import           Metro.TP.BS               (BSHandle, bsTPConfig, closeBSHandle,
                                            feed, newBSHandle)
import           Metro.TP.UDPSocket        (UDPSocket, udpSocket_)
import           Network.Socket            (SockAddr, Socket)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           UnliftIO

data UDPServer = UDPServer Socket (IOHashMap String BSHandle)

instance Servable UDPServer where
  data ServerConfig UDPServer = UDPConfig String
  type SID UDPServer = SockAddr
  type STP UDPServer = UDPSocket
  newServer (UDPConfig hostPort) = do
    sock <- liftIO $ bindTo hostPort
    UDPServer sock <$> HM.empty
  servOnce us@(UDPServer serv handleList) done = do
    (bs, addr) <- liftIO $ recvFrom serv 65535

    bsHandle <- HM.lookup (show addr) handleList
    case bsHandle of
      Just h  -> feed h bs
      Nothing ->
        void . async $ do
          h <- newBSHandle bs
          config <- newTransportConfig us addr h
          done $ Just (addr, config)
          closeBSHandle h

  onConnEnter _ _ = return ()
  onConnLeave (UDPServer _ handleList) addr = HM.delete (show addr) handleList
  servClose (UDPServer serv _) = liftIO $ Socket.close serv

udpServer :: String -> ServerConfig UDPServer
udpServer = UDPConfig

newTransportConfig
  :: (MonadIO m)
  => UDPServer
  -> SockAddr
  -> BSHandle
  -> m (TransportConfig UDPSocket)
newTransportConfig (UDPServer sock handleList) addr h = do
  HM.insert (show addr) h handleList
  return $ udpSocket_ $ bsTPConfig h (flip (sendAllTo sock) addr) $ show addr
