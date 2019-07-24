{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Metro.Example.Types
  ( PacketLength (..)
  , Packet (..)
  ) where

import           Data.Binary          (Binary (..), decode, encode)
import           Data.Binary.Get      (getByteString, getWord16be)
import           Data.Binary.Put      (putByteString, putWord16be)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B (length)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Default.Class   (Default (..))
import           Data.Word            (Word16)
import qualified Metro.Class          as Class


newtype PacketLength = PacketLength Int
  deriving (Show, Eq)

instance Binary PacketLength where
  get = PacketLength . fromIntegral <$> getWord16be
  put (PacketLength l) = putWord16be $ fromIntegral l

data Packet = Packet
  { packetLength :: PacketLength
  , packetId     :: Word16
  , packetBody   :: ByteString
  } deriving (Show, Eq)

instance Binary Packet where
  get = do
    packetLength <- get
    packetId <- getWord16be
    packetBody <- case packetLength of
                    PacketLength l -> getByteString (l - 2)
    return Packet {..}

  put Packet {..} = do
    put packetLength
    putWord16be packetId
    putByteString packetBody

instance Default Packet where
  def = Packet
    { packetLength = PacketLength 0
    , packetId = 0
    , packetBody   = ""
    }

preparePacket :: Packet -> Packet
preparePacket pkt = pkt
  { packetLength =  calcLength pkt
  }

calcLength :: Packet -> PacketLength
calcLength Packet {packetBody = bs} = PacketLength $ B.length bs + 2

instance Class.Packet Packet where
  recvPacket recv = do
    hbs <- recv 2
    case decode (fromStrict hbs) of
      PacketLength len -> do
        bs <- recv len
        return $ decode . fromStrict $ hbs <> bs

  sendPacket pkt send = send . toStrict . encode $ preparePacket pkt

instance Class.PacketId Word16 Packet where
  getPacketId = packetId
  setPacketId k pkt = pkt { packetId = k }
