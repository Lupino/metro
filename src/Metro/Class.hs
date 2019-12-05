{-# LANGUAGE MultiParamTypeClasses #-}

module Metro.Class
  ( RecvPacket (..)
  , SendPacket (..)
  , PacketId (..)
  ) where

import           Data.ByteString (ByteString)
import           UnliftIO        (MonadIO)

class RecvPacket pkt where
  recvPacket :: MonadIO m => (Int -> m ByteString) -> m pkt

class SendPacket pkt where
  sendPacket :: MonadIO m => pkt -> (ByteString -> m ()) -> m ()

class PacketId k pkt where
  getPacketId :: pkt -> k
  setPacketId :: k -> pkt -> pkt
