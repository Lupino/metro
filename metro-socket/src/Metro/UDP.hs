{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.UDP
  ( UDPServer
  , udpConfig
  , newClient
  ) where

import           Data.ByteString           (ByteString, empty)
import           Data.Hashable
import           Metro.Class               (GetPacketId, RecvPacket,
                                            Servable (..), Transport,
                                            TransportConfig)
import           Metro.Conn
import           Metro.IOHashMap           (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap           as HM (delete, insert, lookup)
import           Metro.Node                (NodeEnv1)
import           Metro.Server              (ServerT, getServ, handleConn,
                                            serverEnv)
import           Metro.Session             (SessionT)
import           Metro.Socket              (bindTo, getDatagramAddr)
import           Metro.Transport.BS        (BSHandle, BSTransport,
                                            bsTransportConfig, feed,
                                            newBSHandle)
import           Network.Socket            (SockAddr, Socket, addrAddress)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           System.Log.Logger         (errorM)
import           UnliftIO

data UDPServer = UDPServer Socket (IOHashMap String BSHandle)

instance Servable UDPServer where
  data ServerConfig UDPServer = UDPConfig String
  type SID UDPServer = SockAddr
  type STP UDPServer = BSTransport
  newServer (UDPConfig hostPort) = do
    sock <- liftIO $ bindTo hostPort
    UDPServer sock <$> newIOHashMap
  servOnce us@(UDPServer serv handleList) = do
    (bs, addr) <- liftIO $ recvFrom serv 4194304

    bsHandle <- HM.lookup handleList $ show addr
    case bsHandle of
      Just h  -> feed h bs >> return Nothing
      Nothing -> do
        config <- newTransportConfig us addr bs
        return $ Just (addr, config)

  onConnEnter _ _ = return ()
  onConnLeave (UDPServer _ handleList) addr = HM.delete handleList (show addr)
  servClose (UDPServer serv _) = liftIO $ Socket.close serv

udpConfig :: String -> ServerConfig UDPServer
udpConfig = UDPConfig

newTransportConfig
  :: (MonadIO m)
  => UDPServer
  -> SockAddr
  -> ByteString
  -> m (TransportConfig BSTransport)
newTransportConfig (UDPServer sock handleList) addr bs = do
  h <- newBSHandle bs
  HM.insert handleList (show addr) h
  return $ bsTransportConfig h $ flip (sendAllTo sock) addr

newClient
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => (TransportConfig BSTransport -> TransportConfig tp)
  -> String
  -> nid
  -> u
  -> SessionT u nid k rpkt tp m ()
  -> ServerT UDPServer u nid k rpkt tp m (Maybe (NodeEnv1 u nid k rpkt tp))
newClient mk hostPort nid uEnv sess = do
  addr <- liftIO $ getDatagramAddr hostPort
  case addr of
    Nothing -> do
      liftIO $ errorM "Metro.UDP" $ "Connect UDP Server " ++ hostPort ++ " failed"
      return Nothing
    Just addr0 -> do
      us <- getServ <$> serverEnv
      config <- mk <$> newTransportConfig us (addrAddress addr0) empty
      connEnv <- initConnEnv config
      Just <$> handleConn "Server" (addrAddress addr0) connEnv nid uEnv sess
