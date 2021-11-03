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
  , recvBinary
  , recvDecoder
  , SendPacket (..)
  , sendBinary
  , SetPacketId (..)
  , GetPacketId (..)
  , FindMagic (..)
  , pushChunk
  ) where


import           Control.Exception    (Exception)
import           Data.Binary          (Binary (get), encode)
import           Data.Binary.Get      (Decoder (..), pushChunk,
                                       runGetIncremental)
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
  recvPacket :: MonadUnliftIO m => u -> (ByteString -> m ()) -> (Int -> m ByteString) -> m rpkt
  default recvPacket
    :: (MonadUnliftIO m, FindMagic rpkt, Binary rpkt)
    => u -> (ByteString -> m ()) -> (Int -> m ByteString) -> m rpkt
  recvPacket = recvBinary

class SendPacket spkt where
  sendPacket :: MonadIO m => spkt -> (ByteString -> m ()) -> m ()
  default sendPacket :: (MonadIO m, Binary spkt) => spkt -> (ByteString -> m ()) -> m ()
  sendPacket = sendBinary


class FindMagic rpkt where
  findMagic :: MonadUnliftIO m => Decoder rpkt -> (Int -> m ByteString) -> m (Decoder rpkt)
  findMagic dec _ = return dec


recvBinary
  :: (MonadUnliftIO m, FindMagic rpkt, Binary rpkt)
  => u -> (ByteString -> m ()) -> (Int -> m ByteString) -> m rpkt
recvBinary _ putBack recv = do
  dec <- findMagic (runGetIncremental get) recv
  r <- recvDecoder dec recv
  case r of
    Done buf _ pkt -> putBack buf >> return pkt
    Fail buf _ err -> putBack buf >> error err
    _              -> error "not enough bytes"


recvDecoder :: Monad m => Decoder rpkt -> (Int -> m ByteString) -> m (Decoder rpkt)
recvDecoder (Partial f) recv = do
  bs <- recv 1
  recvDecoder (pushChunk (Partial f) bs) recv
recvDecoder dec _ = return dec

sendBinary :: (MonadIO m, Binary spkt) => spkt -> (ByteString -> m ()) -> m ()
sendBinary spkt send = send . toStrict $ encode spkt

class SetPacketId k pkt where
  setPacketId :: k -> pkt -> pkt

class GetPacketId k pkt where
  getPacketId :: pkt -> k
