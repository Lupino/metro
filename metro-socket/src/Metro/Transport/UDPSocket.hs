{-# LANGUAGE TypeFamilies #-}
module Metro.Transport.UDPSocket
  ( UDPSocket
  , udpSocket
  ) where

import           Control.Monad             (forever)
import           Data.ByteString           (empty)
import           Metro.Class               (Transport (..))
import           Metro.Socket              (bindTo, getDatagramAddr)
import           Metro.Transport.BS        (BSTransport, bsTransportConfig,
                                            feed, newBSHandle)
import           Network.Socket            (addrAddress)
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           System.Log.Logger         (errorM)
import           UnliftIO                  (Async, async, cancel)

data UDPSocket = UDPSocket (Async ()) BSTransport

instance Transport UDPSocket where
  data TransportConfig UDPSocket = SocketUri String
  newTransport (SocketUri h)   = do
    addrInfo <- getDatagramAddr h
    case addrInfo of
      Nothing -> error $ "Connect UDP Server " ++ h ++ " failed"
      Just addrInfo0 -> do
        let addr0 = addrAddress addrInfo0
        bsHandle <- newBSHandle empty
        sock <- bindTo "udp://0.0.0.0:0"

        io <- async $ forever $ do
          (bs, addr1) <- recvFrom sock 1024
          if addr0 == addr1 then feed bsHandle bs
          else errorM "Metro.UDP" $ "Receive unkonw address " ++ show addr1

        tp <- newTransport $ bsTransportConfig bsHandle $ flip (sendAllTo sock) addr0
        return $ UDPSocket io tp

  recvData (UDPSocket _ soc) = recvData soc
  sendData (UDPSocket _ soc) = sendData soc
  closeTransport (UDPSocket io soc) = cancel io >> closeTransport soc

udpSocket :: String -> TransportConfig UDPSocket
udpSocket = SocketUri
