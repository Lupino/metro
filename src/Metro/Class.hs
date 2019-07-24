{-# LANGUAGE MultiParamTypeClasses #-}

module Metro.Class
  ( Packet (..)
  , PacketId (..)
  ) where

import           Data.ByteString (ByteString)
import           UnliftIO        (MonadIO)

class Packet pkt where
  recvPacket :: MonadIO m => (Int -> m ByteString) -> m pkt
  sendPacket :: MonadIO m => pkt -> (ByteString -> m ()) -> m ()

class PacketId k pkt where
  getPacketId :: pkt -> k
  setPacketId :: k -> pkt -> pkt
