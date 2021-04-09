{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.UDPServer
  ( UDPServer
  , udpServer
  , newClient
  ) where

import           Control.Monad             (void)
import           Data.ByteString           (empty)
import           Data.Hashable
import           Data.IOHashMap            (IOHashMap)
import qualified Data.IOHashMap            as HM (delete, empty, insert, lookup)
import           Metro.Class               (GetPacketId, RecvPacket,
                                            Servable (..), Transport,
                                            TransportConfig)
import           Metro.Conn
import           Metro.Node                (NodeEnv1)
import           Metro.Server              (ServerT, getServ, handleConn,
                                            serverEnv)
import           Metro.Session             (SessionT)
import           Metro.Socket              (bindTo, getDatagramAddr)
import           Metro.TP.BS               (BSHandle, bsTransportConfig,
                                            closeBSHandle, feed, newBSHandle)
import           Metro.TP.UDPSocket        (UDPSocket, udpSocket_)
import           Network.Socket            (SockAddr, Socket, addrAddress)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           System.Log.Logger         (errorM)
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
    (bs, addr) <- liftIO $ recvFrom serv 4194304

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
  return $ udpSocket_ $ bsTransportConfig h $ flip (sendAllTo sock) addr

newClient
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => (TransportConfig UDPSocket -> TransportConfig tp)
  -> String
  -> nid
  -> u
  -> (rpkt -> m Bool)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT UDPServer u nid k rpkt tp m (Maybe (NodeEnv1 u nid k rpkt tp))
newClient mk hostPort nid uEnv preprocess sess = do
  addr <- liftIO $ getDatagramAddr hostPort
  case addr of
    Nothing -> do
      liftIO $ errorM "Metro.UDP" $ "Connect UDP Server " ++ hostPort ++ " failed"
      return Nothing
    Just addr0 -> do
      us <- getServ <$> serverEnv
      h <- newBSHandle empty
      config <- mk <$> newTransportConfig us (addrAddress addr0) h
      connEnv <- initConnEnv config
      Just . fst <$> handleConn "Server" (addrAddress addr0) connEnv nid uEnv preprocess sess
