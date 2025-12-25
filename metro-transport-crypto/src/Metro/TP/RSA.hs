{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Metro.TP.RSA
  ( RSA
  , rsa
  , generateKeyPair
  ) where

import           Control.Exception        (displayException)
import           Control.Monad            (unless, (<=<))
import           Crypto.Hash              (Digest, SHA256 (..), hash)
import           Crypto.PubKey.RSA        (PrivateKey, PublicKey, generate)
import           Crypto.PubKey.RSA.OAEP   (OAEPParams, decryptSafer,
                                           defaultOAEPParams, encrypt)
import qualified Crypto.PubKey.RSA.Types  as RSA
import           Data.ASN1.BinaryEncoding (DER (..))
import           Data.ASN1.Encoding       (decodeASN1', encodeASN1')
import           Data.ASN1.Types
import           Data.Bifunctor           (first)
import           Data.ByteArray           (convert)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Either              (fromRight, isRight)
import           Data.List                (find, isSuffixOf)
import           Data.Maybe               (listToMaybe)
import           Data.PEM                 (PEM (..), pemContent, pemParseBS,
                                           pemWriteBS)
import           Metro.Class              (Transport (..))
import           Metro.Utils              (recvEnough)
import           System.Directory         (doesFileExist, listDirectory)
import           System.FilePath          ((</>))
import           UnliftIO                 (TVar, newTVarIO, throwIO, tryAny)

-- | OAEP Configuration: Using SHA256.
-- The overhead for SHA256 OAEP padding is 66 bytes (2 * 32 + 2).
oaepParams :: OAEPParams SHA256 ByteString ByteString
oaepParams = defaultOAEPParams SHA256

oaepSize :: Int
oaepSize = 66

data RSA tp = RSA
  { readBuffer    :: TVar ByteString
  , privateKey    :: PrivateKey
  , peerPublicKey :: PublicKey
  , tp            :: tp
  }

instance (Transport tp) => Transport (RSA tp) where
  data TransportConfig (RSA tp) = RSAConfig PrivateKey FilePath Bool (TransportConfig tp)

  newTP (RSAConfig privateKey publicKeyFileOrDir isClient config) = do
    readBuffer <- newTVarIO BS.empty
    baseTp     <- newTP config

    -- Execute handshake protocol to establish identity
    handshakeResult <- if isClient
      then clientHandshake readBuffer privateKey publicKeyFileOrDir baseTp
      else serverHandshake readBuffer privateKey publicKeyFileOrDir baseTp

    case handshakeResult of
      Nothing -> do
        closeTP baseTp
        throwIO $ userError "RSA Transport Error: Handshake failed. Identity unverified."
      Just peerPub ->
        return RSA
          { peerPublicKey = peerPub
          , tp = baseTp
          , ..
          }

  recvData RSA {..} _ = recvDataOaep readBuffer privateKey tp
  sendData RSA {..}   = sendDataOaep peerPublicKey tp
  closeTP RSA {..}    = closeTP tp
  getTPName RSA {..}  = getTPName tp

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
      -- 1. Send our public key fingerprint (encrypted with server's pub key)
      sendPublicKeyFingerprint myPrivKey serverPub tp
      -- 2. Perform handshake to verify server response/identity
      handshake buf myPrivKey peerKeyPath tp

-- | Server-side handshake: Waits for fingerprint, matches against allowed clients, responds.
serverHandshake :: (Transport tp)
                => TVar ByteString
                -> PrivateKey
                -> FilePath
                -> tp
                -> IO (Maybe PublicKey)
serverHandshake buf myPrivKey authorizedKeysPath tp = do
  -- 1. Receive and verify client fingerprint
  mClientPub <- handshake buf myPrivKey authorizedKeysPath tp
  case mClientPub of
    Nothing -> return Nothing
    Just clientPub -> do
      -- 2. If verified, send back our fingerprint to confirm
      sendPublicKeyFingerprint myPrivKey clientPub tp
      return (Just clientPub)

-- | Core handshake: Loads keys and waits for a fingerprint match.
handshake :: Transport tp => TVar ByteString -> PrivateKey -> FilePath -> tp -> IO (Maybe PublicKey)
handshake buf privKey fileOrDir tp = do
  knownPubKeys <- loadPublicKeys fileOrDir
  incomingFp   <- recvDataOaep buf privKey tp
  return $ findPublicKeyByFingerprint incomingFp knownPubKeys

-- | Encrypts and sends the fingerprint of the local public key.
sendPublicKeyFingerprint :: Transport tp => PrivateKey -> PublicKey -> tp -> IO ()
sendPublicKeyFingerprint myPrivKey peerPubKey tp =
  sendDataOaep peerPubKey tp fingerprint
  where
    fingerprint = publicKeyFingerprint $ RSA.private_pub myPrivKey

---
--- Data Transmission Helpers
---

-- | Receive encrypted data, decrypt it using OAEP.
recvDataOaep :: Transport tp => TVar ByteString -> PrivateKey -> tp -> IO ByteString
recvDataOaep buf privKey tp = do
  encryptedBlob <- recvEnough buf tp size
  -- decryptSafer returns IO (Either RSA.Error ByteString) in OAEP mode
  result <- decryptSafer oaepParams privKey encryptedBlob
  case result of
    Left err        -> throwIO $ userError $ "RSA Decryption Failed: " ++ show err
    Right plainText -> return plainText
  where
    size = RSA.private_size privKey

-- | Encrypt data using OAEP and send it. Handles chunking if data exceeds block size.
sendDataOaep :: Transport tp => PublicKey -> tp -> ByteString -> IO ()
sendDataOaep peerPubKey tp bs
  | BS.null bs = pure ()
  | otherwise = do
      let (chunk, remainder) = BS.splitAt maxSize bs
      -- encrypt returns IO (Either RSA.Error ByteString)
      result <- encrypt oaepParams peerPubKey chunk
      case result of
        Left err         -> throwIO $ userError $ "RSA Encryption Failed: " ++ show err
        Right cipherText -> do
          sendData tp cipherText
          unless (BS.null remainder) $ sendDataOaep peerPubKey tp remainder
  where
    maxSize = RSA.public_size peerPubKey - oaepSize

---
--- Constructor
---

rsa :: FilePath -- ^ Private Key Path
    -> FilePath -- ^ Public Key Path (or Directory for Server)
    -> Bool     -- ^ Is Client?
    -> IO (Either String (TransportConfig tp -> TransportConfig (RSA tp)))
rsa privPath pubPath isClient = do
  ePrivKey <- readPrivateKeyPEM privPath
  return $ case ePrivKey of
    Left err      -> Left err
    Right privKey -> Right $ RSAConfig privKey pubPath isClient

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
  results <- filter isRight <$> mapM readFileWithKeys fullPaths
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
    then fromRight [] <$> readPublicKeyListPEM publicKeyFileOrDir
    else fromRight [] <$> readAllPublicKeysFromDirectory publicKeyFileOrDir

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
