{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Metro.Class
  ( RecvPacket (..)
  , SendPacket (..)
  , sendBinary
  , SetPacketId (..)
  , GetPacketId (..)
  ) where

import           Data.Binary          (Binary, encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           UnliftIO             (MonadIO)

class RecvPacket rpkt where
  recvPacket :: MonadIO m => (Int -> m ByteString) -> m rpkt

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
