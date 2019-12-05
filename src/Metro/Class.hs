{-# LANGUAGE MultiParamTypeClasses #-}

module Metro.Class
  ( RecvPacket (..)
  , SendPacket (..)
  , sendBinary
  , PacketId (..)
  ) where

import           Data.Binary          (Binary, encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           UnliftIO             (MonadIO)

class RecvPacket rpkt where
  recvPacket :: MonadIO m => (Int -> m ByteString) -> m rpkt

class SendPacket spkt where
  sendPacket :: MonadIO m => spkt -> (ByteString -> m ()) -> m ()

sendBinary :: (MonadIO m, Binary spkt) => spkt -> (ByteString -> m ()) -> m ()
sendBinary spkt send = send . toStrict $ encode spkt

class PacketId k pkt where
  getPacketId :: pkt -> k
  setPacketId :: k -> pkt -> pkt
