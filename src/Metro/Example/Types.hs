{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Metro.Example.Types
  ( PacketLength (..)
  , Packet (..)
  , Command (..)
  ) where

import           Data.Binary          (Binary (..), decode, encode, getWord8,
                                       putWord8)
import           Data.Binary.Get      (getByteString,
                                       getRemainingLazyByteString, getWord16be)
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

data Command
  = Run ByteString
  | Upload ByteString ByteString
  | Download ByteString
  | Data ByteString
  deriving (Show, Eq)

instance Binary Command where
  get = do
    cmd <- getWord8
    case cmd of
      1 -> Run . toStrict <$> getRemainingLazyByteString
      2 -> do
        fnL <- fromIntegral <$> getWord8
        fn <- getByteString fnL
        Upload fn . toStrict <$> getRemainingLazyByteString
      3 -> Download . toStrict <$> getRemainingLazyByteString
      4 -> Data . toStrict <$> getRemainingLazyByteString
      _ -> fail "No such command"

  put (Run bs) = do
    putWord8 1
    putByteString bs
  put (Upload fn bs) = do
    putWord8 2
    putWord8 $ fromIntegral $ B.length fn
    putByteString fn
    putByteString bs
  put (Download fn) = do
    putWord8 3
    putByteString fn
  put (Data bs) = do
    putWord8 4
    putByteString bs

calcCommandLength :: Command -> Int
calcCommandLength (Run bs)       = B.length bs + 1
calcCommandLength (Upload fn bs) = B.length fn + B.length bs + 2
calcCommandLength (Download fn)  = B.length fn + 1
calcCommandLength (Data bs)      = B.length bs + 1

data Packet = Packet
  { packetLength :: PacketLength
  , packetId     :: Word16
  , packetCmd    :: Command
  } deriving (Show, Eq)

instance Binary Packet where
  get = do
    packetLength <- get
    packetId     <- getWord16be
    packetCmd    <- get
    return Packet {..}

  put Packet {..} = do
    put packetLength
    putWord16be packetId
    put packetCmd

instance Default Packet where
  def = Packet
    { packetLength = PacketLength 0
    , packetId     = 0
    , packetCmd    = Data ""
    }

preparePacket :: Packet -> Packet
preparePacket pkt = pkt
  { packetLength =  calcLength pkt
  }

calcLength :: Packet -> PacketLength
calcLength Packet {packetCmd = cmd} = PacketLength $ calcCommandLength cmd + 2

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
