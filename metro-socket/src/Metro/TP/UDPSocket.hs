{-# LANGUAGE TypeFamilies #-}
module Metro.TP.UDPSocket
  ( UDPSocket
  , udpSocket
  , udpSocket_
  ) where

import           Control.Monad             (forever)
import           Data.ByteString           (empty)
import           Metro.Class               (Transport (..))
import           Metro.Socket              (bindTo, getDatagramAddr)
import           Metro.TP.BS               (BSTP, bsTPConfig,
                                            feed, newBSHandle)
import           Network.Socket            (addrAddress)
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           System.Log.Logger         (errorM)
import           UnliftIO                  (Async, async, cancel)

data UDPSocket = UDPSocket (Maybe (Async ())) BSTP

instance Transport UDPSocket where
  data TransportConfig UDPSocket =
    RawSocket (TransportConfig BSTP)
    | SocketUri String
  newTP (RawSocket h)   = UDPSocket Nothing <$> newTP h
  newTP (SocketUri h)   = do
    addrInfo <- getDatagramAddr h
    case addrInfo of
      Nothing -> error $ "Connect UDP Server " ++ h ++ " failed"
      Just addrInfo0 -> do
        let addr0 = addrAddress addrInfo0
        bsHandle <- newBSHandle empty
        sock <- bindTo "udp://0.0.0.0:0"

        io <- async $ forever $ do
          (bs, addr1) <- recvFrom sock 4194304
          if addr0 == addr1 then feed bsHandle bs
          else errorM "Metro.UDP" $ "Receive unkonw address " ++ show addr1

        tp <- newTP $ bsTPConfig bsHandle (flip (sendAllTo sock) addr0) $ show addr0
        return $ UDPSocket (Just io) tp

  recvData (UDPSocket _ soc) = recvData soc
  sendData (UDPSocket _ soc) = sendData soc
  closeTP (UDPSocket io soc) = mapM_ cancel io >> closeTP soc
  getTPName (UDPSocket _ soc) = getTPName soc

udpSocket :: String -> TransportConfig UDPSocket
udpSocket = SocketUri

udpSocket_ :: TransportConfig BSTP -> TransportConfig UDPSocket
udpSocket_ = RawSocket
