{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.UDPServer
  ( UDPServer
  , udpServer
  , udpServerWithLimits
  , getSocket
  ) where


import           Control.Monad      (void)
import qualified Data.ByteString    as BS (length)
import           Data.IOMap         (IOMap)
import qualified Data.IOMap         as Map (delete, elems, empty, insert,
                                            lookup, size)
import           Metro.Class        (Servable (..), TransportConfig)
import           Metro.Socket       (bindTo)
import           Metro.TP.BS        (BSHandle, bsTPConfig, closeBSHandle, feed,
                                     newBSHandle_)
import           Metro.TP.UDPSocket (UDPSocket, doSendAll, recvFrom, udpSocket_)
import           Network.Socket     (SockAddr, Socket)
import qualified Network.Socket     as Socket (close)
import           UnliftIO

data UDPServer = UDPServer Socket (IOMap SockAddr BSHandle) UDPServerLimits

data UDPServerLimits = UDPServerLimits
  { maxPeerCount      :: Int
  , maxPeerBufferSize :: Int
  }

defaultUDPServerLimits :: UDPServerLimits
defaultUDPServerLimits = UDPServerLimits
  { maxPeerCount = 1024
  , maxPeerBufferSize = 41943040 -- 40M
  }

instance Servable UDPServer where
  data ServerConfig UDPServer = UDPConfig String UDPServerLimits
  type SID UDPServer = SockAddr
  type STP UDPServer = UDPSocket
  newServer (UDPConfig hostPort limits) = do
    sock <- liftIO $ bindTo hostPort
    UDPServer sock <$> Map.empty <*> pure limits

  servOnce us@(UDPServer serv handleList limits) done = do
    (bs, addr) <- liftIO $ recvFrom serv 65535

    bsHandle <- Map.lookup addr handleList
    case bsHandle of
      Just h  -> feed h bs
      Nothing -> do
        peerCount <- Map.size handleList
        if peerCount >= maxPeerCount limits || BS.length bs > maxPeerBufferSize limits
          then done Nothing
          else do
            -- Register handle before starting async session to avoid duplicate
            -- session creation for packets arriving in a narrow race window.
            h <- newBSHandle_ (maxPeerBufferSize limits) bs
            config <- newTransportConfig us addr h
            void . async $ do
              finally
                (done $ Just (addr, config))
                (Map.delete addr handleList >> closeBSHandle h)

  onConnEnter _ _ = return ()
  onConnLeave (UDPServer _ handleList _) addr = do
    mh <- Map.lookup addr handleList
    mapM_ closeBSHandle mh
    Map.delete addr handleList
  servClose (UDPServer serv handleList _) = do
    hs <- Map.elems handleList
    mapM_ closeBSHandle hs
    void $ liftIO $ tryAny $ Socket.close serv

udpServer :: String -> ServerConfig UDPServer
udpServer hostPort = UDPConfig hostPort defaultUDPServerLimits

udpServerWithLimits :: String -> Int -> Int -> ServerConfig UDPServer
udpServerWithLimits hostPort peerCount bufferSize =
  UDPConfig hostPort UDPServerLimits
    { maxPeerCount = max 1 peerCount
    , maxPeerBufferSize = max 1 bufferSize
    }

newTransportConfig
  :: (MonadIO m)
  => UDPServer
  -> SockAddr
  -> BSHandle
  -> m (TransportConfig UDPSocket)
newTransportConfig (UDPServer sock handleList _) addr h = do
  Map.insert addr h handleList
  return $ udpSocket_ $ bsTPConfig h (doSendAll sock addr) $ show addr

getSocket :: UDPServer -> Socket
getSocket (UDPServer sock _ _) = sock
