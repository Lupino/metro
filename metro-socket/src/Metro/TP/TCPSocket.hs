{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Metro.TP.TCPSocket
  ( TCPSocket
  , tcpSocket
  , tcpSocket_
  ) where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (drop, take)
import           Metro.Class               (Transport (..))
import           Metro.Socket              (connect)
import           Network.Socket            (Socket, close, getPeerName)
import           Network.Socket.ByteString (recv, sendAll)

newtype TCPSocket = TCPSocket Socket

instance Transport TCPSocket where
  data TransportConfig TCPSocket =
      RawSocket Socket
    | SocketUri String
  newTP (RawSocket soc) = pure $ TCPSocket soc
  newTP (SocketUri h)   = TCPSocket <$> connect h
  recvData (TCPSocket soc) = recv soc
  sendData (TCPSocket soc) = doSendAll soc
  closeTP (TCPSocket soc) = close soc
  getTPName (TCPSocket soc) = show <$> getPeerName soc


doSendAll :: Socket -> ByteString -> IO ()
doSendAll _ "" = return ()
doSendAll soc bs = do
  sendAll soc (B.take 8192 bs)
  doSendAll soc (B.drop 8192 bs)

tcpSocket_ :: Socket -> TransportConfig TCPSocket
tcpSocket_ = RawSocket

tcpSocket :: String -> TransportConfig TCPSocket
tcpSocket = SocketUri
