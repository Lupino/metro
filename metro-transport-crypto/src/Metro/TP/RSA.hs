{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Metro.TP.RSA
  ( RSATP
  , RSAMode (..)
  , rsa
  , configClient
  , configServer
  , configServerUnsafePlain
  , generateKeyPair
  ) where

import           Control.Exception        (bracketOnError, displayException)
import           Control.Monad            (unless, when, (<=<))
import           Crypto.Cipher.AES        (AES256)
import           Crypto.Cipher.Types      (Cipher (..), IV, ctrCombine, makeIV)
import           Crypto.Error             (CryptoFailable (..))
import           Crypto.Hash              (Digest, SHA256 (..), hash)
import           Crypto.MAC.HMAC          (HMAC, hmac, hmacGetDigest)
import           Crypto.PubKey.RSA        (PrivateKey, PublicKey, generate)
import           Crypto.PubKey.RSA.OAEP   (OAEPParams, decryptSafer,
                                           defaultOAEPParams, encrypt)
import qualified Crypto.PubKey.RSA.Types  as RSA
import           Data.ASN1.BinaryEncoding (DER (..))
import           Data.ASN1.Encoding       (decodeASN1', encodeASN1')
import           Data.ASN1.Types
import           Data.Bifunctor           (first)
import           Data.Binary              (Binary, decodeOrFail, encode)
import           Data.ByteArray           (constEq, convert)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.List                (find, isSuffixOf)
import           Data.Maybe               (listToMaybe)
import           Data.PEM                 (PEM (..), pemContent, pemParseBS,
                                           pemWriteBS)
import           Data.Word                (Word64)
import           GHC.Generics             (Generic)
import           Metro.Class              (Transport (..))
import           Metro.Utils              (recvEnough)
import           System.Directory         (doesFileExist, listDirectory)
import           System.Entropy           (getEntropy)
import           System.FilePath          ((</>))
import           UnliftIO                 (TVar, atomically, newTVarIO,
                                           readTVarIO, throwIO, tryAny,
                                           writeTVar)

-- | OAEP Configuration: Using SHA256.
-- The overhead for SHA256 OAEP padding is 66 bytes (2 * 32 + 2).
oaepParams :: OAEPParams SHA256 ByteString ByteString
oaepParams = defaultOAEPParams SHA256

oaepSize :: Int
oaepSize = 66

-- | AES Configuration
aesKeySize :: Int
aesKeySize = 32

aesIvSize :: Int
aesIvSize = 16

aesTagSize :: Int
aesTagSize = 32

maxRSAPacketSize :: Int
maxRSAPacketSize = 64 * 1024 * 1024

-- | Transmission Mode
data RSAMode = Plain | RSA | AES
  deriving (Show, Read, Eq, Generic)

instance Binary RSAMode

data RSATP tp = RSATP
  { readBuffer    :: TVar ByteString
  , privateKey    :: PrivateKey
  , peerPublicKey :: PublicKey
  , rsaMode       :: RSAMode
  , sessionKey    :: ByteString -- ^ AES Session Key (Empty if not AES mode)
  , tp            :: tp
  }

instance (Transport tp) => Transport (RSATP tp) where
  data TransportConfig (RSATP tp) = RSAConfig RSAMode [RSAMode] PrivateKey FilePath Bool (TransportConfig tp)

  newTP (RSAConfig configMode allowedModes privateKey publicKeyFileOrDir isClient config) = do
    readBuffer <- newTVarIO BS.empty
    bracketOnError (newTP config) closeTP $ \baseTp -> do

      -- 1. RSA Handshake: Always establish Identity regardless of subsequent mode
      handshakeResult <- if isClient
        then clientHandshake readBuffer privateKey publicKeyFileOrDir baseTp
        else serverHandshake readBuffer privateKey publicKeyFileOrDir baseTp

      case handshakeResult of
        Nothing ->
          throwIO $ userError "RSA Transport Error: Handshake failed. Identity unverified."
        Just peerPub -> do

          actualMode <- if isClient
            then do
              let modeBytes = LBS.toStrict $ encode configMode
              sendDataOaep peerPub baseTp modeBytes
              return configMode
            else do
              modeBytes <- recvDataOaep readBuffer privateKey baseTp
              case decodeOrFail (LBS.fromStrict modeBytes) of
                Left (_, _, err)    -> throwIO $ userError $ "Invalid RSA mode payload: " ++ err
                Right (_, _, cMode) -> do
                  let clientMode = cMode :: RSAMode
                  unless (clientMode `elem` allowedModes) $
                    throwIO $ userError $ "RSA mode not allowed by server policy: " ++ show clientMode
                  return clientMode

          -- 3. Key Exchange: Negotiate AES Session Key ONLY if AES mode is requested
          sKey <- case actualMode of
            AES -> if isClient
              then do
                -- Client generates key and sends it encrypted with Server's PubKey
                k <- getEntropy aesKeySize
                sendDataOaep peerPub baseTp k
                return k
              else do
                -- Server receives the key.
                recvDataOaep readBuffer privateKey baseTp
            _ -> return BS.empty -- No session key needed for Plain or RSA-OAEP modes

          return RSATP
            { peerPublicKey = peerPub
            , tp = baseTp
            , rsaMode = actualMode -- Use the negotiated mode
            , sessionKey = sKey
            , ..
            }

  recvData RSATP {..} n = case rsaMode of
    Plain -> recvDataPlain readBuffer tp n
    RSA   -> recvDataOaep readBuffer privateKey tp -- Legacy/Fallback mode
    AES   -> recvDataAes readBuffer sessionKey tp n

  sendData RSATP {..} bs = case rsaMode of
    Plain -> sendData tp bs
    RSA   -> sendDataOaep peerPublicKey tp bs
    AES   -> sendDataAes sessionKey tp bs

  closeTP RSATP {..}    = closeTP tp
  getTPName RSATP {..}  = getTPName tp

---
--- Handshake Logic
---

-- | Client-side handshake: Loads trusted keys, sends own fingerprint, expects server acceptance.
clientHandshake :: (Transport tp)
                => TVar ByteString
                -> PrivateKey
                -> FilePath
                -> tp
                -> IO (Maybe PublicKey)
clientHandshake buf myPrivKey peerKeyPath tp = do
  keys <- loadPublicKeys peerKeyPath
  case listToMaybe keys of
    Nothing -> do
      throwIO $ userError "RSA Config: No public keys found for client (Server key needed)."
    Just serverPub -> do
      nonce <- getEntropy handshakeNonceSize
      let myFp = publicKeyFingerprint $ RSA.private_pub myPrivKey
      sendDataOaep serverPub tp $ myFp `BS.append` nonce
      serverFp <- recvDataOaep buf myPrivKey tp
      proof <- recvDataOaep buf myPrivKey tp
      challenge <- recvDataOaep buf myPrivKey tp
      case findPublicKeyByFingerprint serverFp keys of
        Nothing -> return Nothing
        Just verifiedServerPub -> do
          let expected = handshakeHash ["metro-rsa-server-v1", nonce, serverFp]
          unless (proof == expected) $
            throwIO $ userError "RSA Transport Error: server proof verification failed"
          sendDataOaep verifiedServerPub tp $
            handshakeHash ["metro-rsa-client-v1", challenge, myFp]
          return $ Just verifiedServerPub

-- | Server-side handshake: Waits for fingerprint, matches against allowed clients, responds.
serverHandshake :: (Transport tp)
                => TVar ByteString
                -> PrivateKey
                -> FilePath
                -> tp
                -> IO (Maybe PublicKey)
serverHandshake buf myPrivKey authorizedKeysPath tp = do
  knownPubKeys <- loadPublicKeys authorizedKeysPath
  helloBytes <- recvDataOaep buf myPrivKey tp
  let (incomingFp, nonce) = BS.splitAt fingerprintSize helloBytes
  unless (BS.length incomingFp == fingerprintSize && BS.length nonce == handshakeNonceSize) $
    throwIO $ userError "RSA Transport Error: invalid client hello length"
  case findPublicKeyByFingerprint incomingFp knownPubKeys of
    Nothing -> return Nothing
    Just clientPub -> do
      challenge <- getEntropy handshakeNonceSize
      let myFp = publicKeyFingerprint $ RSA.private_pub myPrivKey
          proof = handshakeHash ["metro-rsa-server-v1", nonce, myFp]
      sendDataOaep clientPub tp myFp
      sendDataOaep clientPub tp proof
      sendDataOaep clientPub tp challenge
      clientProof <- recvDataOaep buf myPrivKey tp
      let expected = handshakeHash ["metro-rsa-client-v1", challenge, incomingFp]
      if clientProof == expected
        then return $ Just clientPub
        else throwIO $ userError "RSA Transport Error: client proof verification failed"

handshakeNonceSize :: Int
handshakeNonceSize = 16

fingerprintSize :: Int
fingerprintSize = 32

handshakeHash :: [ByteString] -> ByteString
handshakeHash parts = convert (hash (BS.concat parts) :: Digest SHA256)

---
--- Plain Data Helpers (Fixing Buffer Issue)
---

-- | Read data: Prioritize reading from the buffer.
-- If the buffer is empty, read from the underlying Transport.
-- This resolves the issue where excessive reading during the handshake phase
-- causes data to remain in the Buffer and be ignored by a raw recvData call.
recvDataPlain :: (Transport tp) => TVar ByteString -> tp -> Int -> IO ByteString
recvDataPlain buf tp n = do
  currentBuf <- readTVarIO buf
  if BS.null currentBuf
    then recvData tp n -- Buffer is empty, read directly from network
    else do
      -- Buffer has data, consume from buffer first
      let (ret, rest) = BS.splitAt n currentBuf
      atomically $ writeTVar buf rest
      return ret

---
--- AES Data Transmission (Hybrid)
---

-- | Initialize AES-256 Cipher
initAES :: ByteString -> IV AES256 -> IO AES256
initAES key _ = case cipherInit key of
  CryptoFailed e -> throwIO . userError $ "AES Init Failed: " ++ show e
  CryptoPassed c -> pure c

-- | Encrypts data using AES-256-CTR and sends it with a length header.
-- Format: [Length (8 bytes)][IV (16 bytes)][Ciphertext]
sendDataAes :: (Transport tp) => ByteString -> tp -> ByteString -> IO ()
sendDataAes key tp bs
  | BS.null bs = return ()
  | BS.length bs + aesIvSize + aesTagSize > maxRSAPacketSize =
      throwIO $ userError "AES packet length exceeds maximum"
  | otherwise = do
      ivBytes <- getEntropy aesIvSize
      iv <- case makeIV ivBytes of
              Just i  -> pure i
              Nothing -> throwIO (userError "Failed to generate IV")

      ctx <- initAES key iv
      let cipherText = ctrCombine ctx iv bs
          tag = hmacSHA256 key (ivBytes `BS.append` cipherText)
          payload = BS.concat [ivBytes, cipherText, tag]
          -- Length header (Word64 -> 8 bytes)
          lenBytes = LBS.toStrict $ encode (fromIntegral (BS.length payload) :: Word64)

      sendData tp (lenBytes `BS.append` payload)

-- | Receives AES encrypted data. Handles buffering and packet reassembly.
recvDataAes :: (Transport tp) => TVar ByteString -> ByteString -> tp -> Int -> IO ByteString
recvDataAes buf key tp _ = do
      -- 1. Read packet length (8 bytes Word64)
  lenBytes <- recvEnough buf tp 8
  pktLen64 <- case decodeOrFail (LBS.fromStrict lenBytes) of
    Left (_, _, err) -> throwIO $ userError $ "Invalid AES length header: " ++ err
    Right (_, _, v)  -> pure (v :: Word64)
  when (pktLen64 < fromIntegral (aesIvSize + aesTagSize) || pktLen64 > fromIntegral maxRSAPacketSize) $
    throwIO $ userError "Invalid AES packet length"
  let pktLen = fromIntegral pktLen64 :: Int

  -- 2. Read Packet Payload (IV + Ciphertext)
  payload <- recvEnough buf tp pktLen
  let (ivBytes, cipherTextAndTag) = BS.splitAt aesIvSize payload
      cipherTextSize = BS.length cipherTextAndTag - aesTagSize
      (cipherText, tag) = BS.splitAt cipherTextSize cipherTextAndTag
      expectedTag = hmacSHA256 key (ivBytes `BS.append` cipherText)
  unless (tag `constEq` expectedTag) $
    throwIO $ userError "Invalid AES packet authentication tag"

  -- 3. Decrypt
  iv <- case makeIV ivBytes of
          Just i  -> pure i
          Nothing -> throwIO (userError "Invalid IV received")

  ctx <- initAES key iv
  return $ ctrCombine ctx iv cipherText

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = convert (hmacGetDigest (hmac key msg :: HMAC SHA256))

---
--- RSA Data Transmission Helpers (OAEP)
---

-- | Receive encrypted data, decrypt it using OAEP.
recvDataOaep :: Transport tp => TVar ByteString -> PrivateKey -> tp -> IO ByteString
recvDataOaep buf privKey tp = do
  encryptedBlob <- recvEnough buf tp size
  result <- decryptSafer oaepParams privKey encryptedBlob
  case result of
    Left err        -> throwIO $ userError $ "RSA Decryption Failed: " ++ show err
    Right plainText -> return plainText
  where
    size = RSA.private_size privKey

-- | Encrypt data using OAEP and send it.
-- OAEP transport reads one RSA block at a time, so payload must fit in one block.
sendDataOaep :: Transport tp => PublicKey -> tp -> ByteString -> IO ()
sendDataOaep peerPubKey tp bs
  | BS.null bs = pure ()
  | BS.length bs > maxSize =
      throwIO $ userError "RSA payload too large for OAEP mode; use AES or Plain mode"
  | otherwise = do
      result <- encrypt oaepParams peerPubKey bs
      case result of
        Left err         -> throwIO $ userError $ "RSA Encryption Failed: " ++ show err
        Right cipherText -> sendData tp cipherText
  where
    maxSize = RSA.public_size peerPubKey - oaepSize

---
--- Constructor & Config Helpers
---

-- | Updated constructor: Takes RSAMode as argument
rsa :: RSAMode  -- ^ Transmission Mode (Plain, RSA, or AES)
    -> FilePath -- ^ Private Key Path
    -> FilePath -- ^ Public Key Path (or Directory for Server)
    -> Bool     -- ^ Is Client?
    -> IO (TransportConfig tp -> TransportConfig (RSATP tp))
rsa mode privPath pubPath isClient = do
  ePrivKey <- readPrivateKeyPEM privPath
  case ePrivKey of
    Left err       -> throwIO $ userError $ "Read RSA private key failed: " ++ show err
    Right privKey -> return $ RSAConfig mode (defaultAllowedModes isClient) privKey pubPath isClient

-- | Client Configuration: Explicitly sets the RSAMode
configClient :: RSAMode  -- ^ Mode to negotiate with server (Plain, RSA, AES)
             -> FilePath -- ^ Client Private Key
             -> FilePath -- ^ Server Public Key
             -> IO (TransportConfig tp -> TransportConfig (RSATP tp))
configClient mode privPath pubPath = rsa mode privPath pubPath True

-- | Server Configuration: Mode is determined by the Client
configServer :: FilePath -- ^ Server Private Key
             -> FilePath -- ^ Directory containing authorized Client Public Keys
             -> IO (TransportConfig tp -> TransportConfig (RSATP tp))
configServer privPath pubDir = rsa Plain privPath pubDir False

configServerUnsafePlain :: FilePath
                        -> FilePath
                        -> IO (TransportConfig tp -> TransportConfig (RSATP tp))
configServerUnsafePlain privPath pubDir = do
  ePrivKey <- readPrivateKeyPEM privPath
  case ePrivKey of
    Left err      -> throwIO $ userError $ "Read RSA private key failed: " ++ show err
    Right privKey -> return $ RSAConfig Plain [Plain, RSA, AES] privKey pubDir False

defaultAllowedModes :: Bool -> [RSAMode]
defaultAllowedModes True  = [Plain, RSA, AES]
defaultAllowedModes False = [RSA, AES]

---
--- PEM & Key Management
---

-- | Helper to calculate byte size from bit size integer.
numBytes :: Integer -> Int
numBytes n = (integerLog2 n + 7) `div` 8

-- | Simple integer log2 implementation.
integerLog2 :: Integer -> Int
integerLog2 0 = 0
integerLog2 n = 1 + integerLog2 (n `div` 2)

pemToPrivateKey :: ByteString -> Either String PrivateKey
pemToPrivateKey pemBS = do
  pems <- case pemParseBS pemBS of
    Left err    -> Left err
    Right []    -> Left "No PEM data found"
    Right (p:_) -> Right p

  asn1 <- case decodeASN1' DER (pemContent pems) of
    Left err -> Left (show err)
    Right a  -> Right a

  case asn1 of
    (Start Sequence : IntVal _ : IntVal n : IntVal e : IntVal d :
     IntVal p : IntVal q : IntVal dP : IntVal dQ : IntVal qinv : End Sequence : _) ->
        Right $ RSA.PrivateKey
          { RSA.private_pub = RSA.PublicKey (fromIntegral $ numBytes n) n e
          , RSA.private_d = d
          , RSA.private_p = p
          , RSA.private_q = q
          , RSA.private_dP = dP
          , RSA.private_dQ = dQ
          , RSA.private_qinv = qinv
          }
    _ -> Left "Invalid ASN.1 structure for RSA private key"

readPrivateKeyPEM :: FilePath -> IO (Either String PrivateKey)
readPrivateKeyPEM path =
  (pemToPrivateKey <=< first displayException) <$> tryAny (BS.readFile path)

pemToPublicKeyList :: ByteString -> Either String [PublicKey]
pemToPublicKeyList pemBS = do
  pems <- case pemParseBS pemBS of
    Left err -> Left err
    Right [] -> Left "No PEM data found"
    Right ps -> Right ps
  mapM parsePEM pems
  where
    parsePEM pem = do
      asn1 <- case decodeASN1' DER (pemContent pem) of
        Left err -> Left (show err)
        Right a  -> Right a
      case asn1 of
        (Start Sequence : IntVal n : IntVal e : End Sequence : _) ->
          Right $ RSA.PublicKey (fromIntegral $ numBytes n) n e
        _ -> Left "Invalid ASN.1 structure for RSA public key"

readPublicKeyListPEM :: FilePath -> IO (Either String [PublicKey])
readPublicKeyListPEM path =
  (pemToPublicKeyList <=< first displayException) <$> tryAny (BS.readFile path)

readPublicKeysFromDirectory :: FilePath -> IO (Either String [(FilePath, [PublicKey])])
readPublicKeysFromDirectory dir = do
  allFiles <- listDirectory dir
  let pemFiles = filter (".pem" `isSuffixOf`) allFiles
      fullPaths = map (dir </>) pemFiles
  results <- mapM readFileWithKeys fullPaths
  return $ sequence results
  where
    readFileWithKeys path = do
      keysResult <- readPublicKeyListPEM path
      return $ case keysResult of
        Right keys -> Right (path, keys)
        Left err   -> Left (path ++ ": " ++ err)

readAllPublicKeysFromDirectory :: FilePath -> IO (Either String [PublicKey])
readAllPublicKeysFromDirectory dir = do
  result <- readPublicKeysFromDirectory dir
  return $ case result of
    Right fileKeys -> Right $ concatMap snd fileKeys
    Left err       -> Left err

loadPublicKeys :: FilePath -> IO [PublicKey]
loadPublicKeys publicKeyFileOrDir = do
  isFile <- doesFileExist publicKeyFileOrDir
  if isFile
    then do
      r <- readPublicKeyListPEM publicKeyFileOrDir
      case r of
        Right ks -> pure ks
        Left err -> ioError $ userError $ "Read RSA public key file failed: " ++ err
    else do
      r <- readAllPublicKeysFromDirectory publicKeyFileOrDir
      case r of
        Right ks -> pure ks
        Left err -> ioError $ userError $ "Read RSA public key directory failed: " ++ err

publicKeyFingerprint :: PublicKey -> ByteString
publicKeyFingerprint key = convert (hash asn1Bytes :: Digest SHA256)
  where
    asn1Bytes = publicKeyToASN1 key

findPublicKeyByFingerprint :: ByteString -> [PublicKey] -> Maybe PublicKey
findPublicKeyByFingerprint fp = find (\k -> publicKeyFingerprint k == fp)

-- | Serializes a Private Key to PEM format.
privateKeyToPEM :: PrivateKey -> ByteString
privateKeyToPEM key = pemWriteBS pem
  where
    pem = PEM "RSA PRIVATE KEY" [] (encodeASN1' DER asn1)
    asn1 =
      [ Start Sequence, IntVal 0
      , IntVal (RSA.private_n key)
      , IntVal (RSA.public_e $ RSA.private_pub key)
      , IntVal (RSA.private_d key)
      , IntVal (RSA.private_p key)
      , IntVal (RSA.private_q key)
      , IntVal (RSA.private_dP key)
      , IntVal (RSA.private_dQ key)
      , IntVal (RSA.private_qinv key)
      , End Sequence
      ]

publicKeyToASN1 :: PublicKey -> ByteString
publicKeyToASN1 key = encodeASN1' DER
  [ Start Sequence
  , IntVal (RSA.public_n key)
  , IntVal (RSA.public_e key)
  , End Sequence
  ]

publicKeyToPEM :: PublicKey -> ByteString
publicKeyToPEM key = pemWriteBS $ PEM "RSA PUBLIC KEY" [] (publicKeyToASN1 key)

writePrivateKeyPEM :: FilePath -> PrivateKey -> IO ()
writePrivateKeyPEM path key = BS.writeFile path (privateKeyToPEM key)

writePublicKeyPEM :: FilePath -> PublicKey -> IO ()
writePublicKeyPEM path key = BS.writeFile path (publicKeyToPEM key)

-- | Generates an RSA keypair of the given size (bits) and saves them to files.
generateKeyPair :: FilePath -> Int -> IO ()
generateKeyPair prefix size = do
  (pubKey, privKey) <- generate size 65537
  writePrivateKeyPEM privFile privKey
  writePublicKeyPEM pubFile pubKey
  putStrLn $ "Keys written to " ++ privFile ++ " and " ++ pubFile
  where
    privFile = prefix ++ "_private_key.pem"
    pubFile  = prefix ++ "_public_key.pem"
