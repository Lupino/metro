{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Metro.TP.Crypto
  ( Crypto
  , crypto
  , crypto_
  , CryptoMethod (..)
  , methodEcb
  , methodCbc
  , methodCfb
  , methodCtr

  , makeCrypto
  ) where

import           Control.Monad        (when)
import           Crypto.Cipher.Types  (BlockCipher (..), Cipher (..), IV (..),
                                       KeySizeSpecifier (..), ivAdd, nullIV)
import           Crypto.Error         (CryptoFailable (..))
import           Data.Binary          (Binary (..), decode, encode)
import           Data.Binary.Get      (getByteString, getWord32be)
import           Data.Binary.Put      (putByteString, putWord32be)
import           Data.ByteString      (ByteString, empty)
import qualified Data.ByteString      as B (length, take)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LB (cycle, fromStrict, take, toStrict)
import qualified Data.Text            as T (pack)
import           Data.Text.Encoding   (encodeUtf8)
import           Metro.Class          (Transport (..))
import           Metro.Utils          (recvEnough)
import           UnliftIO

newtype BlockLength = BlockLength Int
  deriving (Show, Eq)

instance Binary BlockLength where
  get = BlockLength . fromIntegral <$> getWord32be
  put (BlockLength l) = putWord32be $ fromIntegral l

data Block = Block
  { msgSize :: !Int
  , encData :: !ByteString
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

fixedLengthR :: Int -> ByteString -> ByteString
fixedLengthR m bs | B.length bs < m = fixedLengthR m $ bs <> "0"
                  | B.length bs > m = fixedLengthR m $ B.take (B.length bs - 1) bs
                  | otherwise = bs

makeBlock :: Int -> ByteString -> Block
makeBlock bSize msg = Block size $ fixedLengthR fixedSize msg
  where size = B.length msg
        fixedSize = (ceiling (fromIntegral size / fromIntegral bSize * 1.0)) * bSize

getMsg :: Block -> ByteString
getMsg Block {..} = B.take msgSize encData

prepareBlock
  :: BlockCipher cipher
  => (cipher -> IV cipher -> ByteString -> ByteString)
  -> cipher -> IV cipher -> Block -> Block
prepareBlock f c iv b = b { encData = f c iv (encData b) }

data CryptoMethod cipher = CryptoMethod
  { encrypt :: cipher -> IV cipher -> ByteString -> ByteString
  , decrypt :: cipher -> IV cipher -> ByteString -> ByteString
  , needIV  :: Bool
  }

data Crypto cipher tp = Crypto
  { readBuffer   :: TVar ByteString
  , cryptoMethod :: CryptoMethod cipher
  , readIV       :: TVar (IV cipher)
  , writeIV      :: TVar (IV cipher)
  , cipher       :: cipher
  , tp           :: tp
  }

instance (Transport tp, BlockCipher cipher) => Transport (Crypto cipher tp) where
  data TransportConfig (Crypto cipher tp) =
    CryptoConfig (CryptoMethod cipher) cipher (IV cipher) (TransportConfig tp)
  newTransport (CryptoConfig cryptoMethod cipher iv config) = do
    readBuffer <- newTVarIO empty
    tp <- newTransport config
    readIV  <- newTVarIO iv
    writeIV <- newTVarIO iv
    return Crypto {..}
  recvData (Crypto buf method ivr _ cipher tp) _ = do
    hbs <- recvEnough buf tp 4
    iv <- readTVarIO ivr
    case decode (fromStrict hbs) of
      BlockLength len -> do
        bs <- getMsg
          . prepareBlock (decrypt method) cipher iv
          . decode
          . fromStrict
          . (hbs <>) <$> recvEnough buf tp len

        when (needIV method) $
          atomically $ writeTVar ivr (ivAdd iv (B.length bs))

        return bs
  sendData (Crypto _ method _ ivw cipher tp) bs = do
    iv <- readTVarIO ivw

    when (needIV method) $
      atomically $ writeTVar ivw (ivAdd iv (B.length bs))

    sendData tp
      . toStrict
      . encode
      . prepareBlock (encrypt method) cipher iv
      $ makeBlock (blockSize cipher) bs
  closeTransport (Crypto _ _ _ _ _ tp) = closeTransport tp

crypto
  :: BlockCipher cipher
  => CryptoMethod cipher
  -> cipher
  -> TransportConfig tp
  -> TransportConfig (Crypto cipher tp)
crypto method cipher = crypto_ method cipher nullIV
crypto_
  :: BlockCipher cipher
  => CryptoMethod cipher
  -> cipher
  -> IV cipher
  -> TransportConfig tp
  -> TransportConfig (Crypto cipher tp)
crypto_ = CryptoConfig

methodEcb :: BlockCipher cipher => CryptoMethod cipher
methodEcb = CryptoMethod (ignoreIV ecbEncrypt) (ignoreIV ecbDecrypt) False
  where ignoreIV f c _ = f c

methodCbc :: BlockCipher cipher => CryptoMethod cipher
methodCbc = CryptoMethod cbcEncrypt cbcDecrypt True

methodCfb :: BlockCipher cipher => CryptoMethod cipher
methodCfb = CryptoMethod cfbEncrypt cfbDecrypt  True

methodCtr :: BlockCipher cipher => CryptoMethod cipher
methodCtr = CryptoMethod ctrCombine ctrCombine True

getCryptoMethod :: BlockCipher cipher => cipher -> String -> Maybe (CryptoMethod cipher)
getCryptoMethod _ "CBC" = Just methodCbc
getCryptoMethod _ "cbc" = Just methodCbc
getCryptoMethod _ "CFB" = Just methodCfb
getCryptoMethod _ "cfb" = Just methodCfb
getCryptoMethod _ "ECB" = Just methodEcb
getCryptoMethod _ "ecb" = Just methodEcb
getCryptoMethod _ "CTR" = Just methodCtr
getCryptoMethod _ "ctr" = Just methodCtr
getCryptoMethod _ _     = Nothing

makeCrypto
  :: forall cipher tp. (BlockCipher cipher, Cipher cipher)
  => cipher -> String -> String -> TransportConfig tp -> TransportConfig (Crypto cipher tp)
makeCrypto cipher method key c =
  case getCryptoMethod cipher method of
    Nothing -> error "crypto method not support"
    Just m  ->
      case cipherInit key0 of
        CryptoFailed e         -> error $ "Cipher init failed " ++ show e
        CryptoPassed (newCipher :: cipher) ->
          crypto m newCipher c

  where size = getKeySize $ cipherKeySize cipher
        key0 =
          LB.toStrict
          . LB.take (fromIntegral size)
          . LB.cycle
          . LB.fromStrict
          . encodeUtf8
          $ T.pack key


getKeySize :: KeySizeSpecifier -> Int
getKeySize (KeySizeRange _ x) = x
getKeySize (KeySizeEnum xs)   = maximum xs
getKeySize (KeySizeFixed x)   = x
