{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Metro.TP.UDPSocket
  ( UDPSocket
  , udpSocket
  , udpSocket_
  , doSendAll
  , recvFrom
  ) where

import           Control.Monad             (forever)
import           Data.ByteString           (ByteString, empty)
import qualified Data.ByteString           as B (drop, take)
import           Metro.Class               (Transport (..))
import           Metro.Socket              (bindTo, getDatagramAddr)
import           Metro.TP.BS               (BSTP, bsTPConfig, feed, newBSHandle)
import           Network.Socket            (SockAddr (..), Socket, addrAddress)
import qualified Network.Socket            as Socket (close)
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           System.Log.Logger         (errorM)
import           UnliftIO                  (Async, async, cancel, throwIO)

data UDPSocket = UDPSocket (Maybe (Async ())) (Maybe Socket) BSTP

instance Transport UDPSocket where
  data TransportConfig UDPSocket =
    RawSocket (TransportConfig BSTP)
    | SocketUri String
  newTP (RawSocket h)   = UDPSocket Nothing Nothing <$> newTP h
  newTP (SocketUri h)   = do
    addrInfo <- getDatagramAddr h
    case addrInfo of
      Nothing -> throwIO . userError $ "Connect UDP Server " ++ h ++ " failed"
      Just addrInfo0 -> do
        let addr0 = addrAddress addrInfo0
        bsHandle <- newBSHandle empty
        sock <- bindTo $ case addr0 of
          SockAddrInet6 {} -> "udp://[::]:0"
          _                -> "udp://0.0.0.0:0"

        io <- async $ forever $ do
          (bs, addr1) <- recvFrom sock 65535
          if addr0 == addr1 then feed bsHandle bs
          else errorM "Metro.UDP" $ "Receive unkonw address " ++ show addr1

        tp <- newTP $ bsTPConfig bsHandle (doSendAll sock addr0) $ show addr0
        return $ UDPSocket (Just io) (Just sock) tp

  recvData (UDPSocket _ _ soc) = recvData soc
  sendData (UDPSocket _ _ soc) = sendData soc
  closeTP (UDPSocket io sock soc) = do
    mapM_ cancel io
    mapM_ Socket.close sock
    closeTP soc
  getTPName (UDPSocket _ _ soc) = getTPName soc

doSendAll :: Socket -> SockAddr -> ByteString -> IO ()
doSendAll _ _ "" = return ()
doSendAll soc addr bs = do
  sendAllTo soc (B.take 8192 bs) addr
  doSendAll soc addr (B.drop 8192 bs)

udpSocket :: String -> TransportConfig UDPSocket
udpSocket = SocketUri

udpSocket_ :: TransportConfig BSTP -> TransportConfig UDPSocket
udpSocket_ = RawSocket
