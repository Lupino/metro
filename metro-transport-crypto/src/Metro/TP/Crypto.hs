{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Metro.TP.Crypto
  ( Crypto
  , crypto
  , CryptoMethod (..)
  ) where

import           Crypto.Cipher.Types  (BlockCipher (..))
import           Data.Binary          (Binary (..), decode, encode)
import           Data.Binary.Get      (getByteString, getWord32be)
import           Data.Binary.Put      (putByteString, putWord32be)
import           Data.ByteString      (ByteString, empty)
import qualified Data.ByteString      as B (drop, length, take)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Metro.Class          (Transport (..))
import           Metro.Utils          (recvEnough)
import           UnliftIO

newtype BlockLength = BlockLength Int
  deriving (Show, Eq)

instance Binary BlockLength where
  get = BlockLength . fromIntegral <$> getWord32be
  put (BlockLength l) = putWord32be $ fromIntegral l

data Block = Block
  { msgSize :: Int
  , encData :: ByteString
  }
  deriving (Show, Eq)

instance Binary Block where
  get = do
    pktSize <- fromIntegral <$> getWord32be
    msgSize <- fromIntegral <$> getWord32be
    encData <- getByteString $ pktSize - 4
    return Block {..}
  put Block {..} = do
    putWord32be $ fromIntegral $ B.length encData + 4
    putWord32be $ fromIntegral msgSize
    putByteString encData


fixedLength :: Int -> ByteString -> ByteString
fixedLength m bs | B.length bs < m = fixedLength m $ "0" <> bs
                 | B.length bs > m = fixedLength m $ B.drop 1 bs
                 | otherwise = bs

makeBlock :: Int -> ByteString -> Block
makeBlock bSize msg = Block size $ fixedLength fixedSize msg
  where size = B.length msg
        fixedSize = (ceiling (fromIntegral size / fromIntegral bSize * 1.0)) * bSize

getMsg :: Block -> ByteString
getMsg Block {..} = B.take msgSize encData

prepareBlock :: BlockCipher cipher => (cipher -> ByteString -> ByteString) -> cipher -> Block -> Block
prepareBlock f c b = b { encData = f c (encData b) }

data CryptoMethod cipher = CryptoMethod
  { encrypt :: cipher -> ByteString -> ByteString
  , decrypt :: cipher -> ByteString -> ByteString
  }

data Crypto cipher tp = Crypto (TVar ByteString) (CryptoMethod cipher) cipher tp

instance (Transport tp, BlockCipher cipher) => Transport (Crypto cipher tp) where
  data TransportConfig (Crypto cipher tp) = CryptoConfig (CryptoMethod cipher) cipher (TransportConfig tp)
  newTransport (CryptoConfig method cipher config) = do
    buf <- newTVarIO empty
    Crypto buf method cipher <$> newTransport config
  recvData (Crypto buf method cipher tp) _ = do
    hbs <- recvEnough buf tp 4
    case decode (fromStrict hbs) of
      BlockLength len -> do
        bs <- recvEnough buf tp len
        return . getMsg . prepareBlock (decrypt method) cipher . decode . fromStrict $ hbs <> bs
  sendData (Crypto _ method cipher tp) =
    sendData tp
      . toStrict
      . encode
      . prepareBlock (encrypt method) cipher
      . makeBlock (blockSize cipher)
  closeTransport (Crypto _ _ _ tp) = closeTransport tp

crypto :: CryptoMethod cipher -> cipher -> TransportConfig tp -> TransportConfig (Crypto cipher tp)
crypto = CryptoConfig
