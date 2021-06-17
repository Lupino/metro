{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Metro.Class
  ( Transport (..)
  , TransportError (..)
  , Servable (..)
  , RecvPacket (..)
  , SendPacket (..)
  , sendBinary
  , SetPacketId (..)
  , GetPacketId (..)
  ) where

import           Control.Exception    (Exception)
import           Data.Binary          (Binary, encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           UnliftIO             (MonadIO, MonadUnliftIO)

data TransportError = TransportClosed
    deriving (Show, Eq, Ord)

instance Exception TransportError

class Transport transport where
  data TransportConfig transport
  newTP     :: TransportConfig transport -> IO transport
  recvData  :: transport -> Int -> IO ByteString
  sendData  :: transport -> ByteString -> IO ()
  closeTP   :: transport -> IO ()
  getTPName :: transport -> IO String

class Servable serv where
  data ServerConfig serv
  type SID serv
  type STP serv
  newServer   :: MonadIO m => ServerConfig serv -> m serv
  servOnce    :: MonadUnliftIO m => serv -> (Maybe (SID serv, TransportConfig (STP serv)) -> m ()) -> m ()
  onConnEnter :: MonadIO m => serv -> SID serv -> m ()
  onConnLeave :: MonadIO m => serv -> SID serv -> m ()
  servClose   :: MonadIO m => serv -> m ()

class RecvPacket u rpkt where
  recvPacket :: MonadUnliftIO m => u -> (Int -> m ByteString) -> m rpkt

class SendPacket spkt where
  sendPacket :: MonadIO m => spkt -> (ByteString -> m ()) -> m ()
  default sendPacket :: (MonadIO m, Binary spkt) => spkt -> (ByteString -> m ()) -> m ()
  sendPacket = sendBinary

sendBinary :: (MonadIO m, Binary spkt) => spkt -> (ByteString -> m ()) -> m ()
sendBinary spkt send = send . toStrict $ encode spkt

class SetPacketId k pkt where
  setPacketId :: k -> pkt -> pkt

class GetPacketId k pkt where
  getPacketId :: pkt -> k
