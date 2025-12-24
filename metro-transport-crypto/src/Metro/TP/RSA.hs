{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Metro.TP.RSA
  ( RSA
  , rsa
  , generateKeyPair
  ) where

import           Control.Monad            (unless)
import           Crypto.Hash              (Digest, SHA256 (..), hash)
import           Crypto.PubKey.RSA        (PrivateKey, PublicKey, generate)
import           Crypto.PubKey.RSA.OAEP   (decryptSafer, defaultOAEPParams,
                                           encrypt)
import qualified Crypto.PubKey.RSA.Types  as RSA
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types
import           Data.ByteArray           (convert)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Either              (fromRight)
import           Data.List                (find, isSuffixOf)
import           Data.Maybe               (listToMaybe)
import           Data.PEM                 (PEM (..), pemContent, pemParseBS,
                                           pemWriteBS)
import           Metro.Class              (Transport (..))
import           Metro.Utils              (recvEnough)
import           System.Directory         (doesFileExist, listDirectory)
import           System.FilePath          ((</>))
import           UnliftIO

-- OAEP 配置：使用 SHA256 算法。
-- SHA256 的 OAEP 填充开销为 66 字节（2 * 32 + 2）。
oaepParams = defaultOAEPParams SHA256
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
    tp <- newTP config

    -- 握手逻辑
    mPubKey <- if isClient
      then do -- 客户端：加载公钥并发送指纹
        keys <- loadPublicKeys publicKeyFileOrDir
        case listToMaybe keys of
          Nothing -> do
            closeTP tp
            throwIO $ userError "RSA Config: No public keys found for client."
          Just pub -> do
            sendPublicKeyFingerprint privateKey pub tp
            handshake readBuffer privateKey publicKeyFileOrDir tp
      else do -- 服务端：校验客户端指纹并回应
        p <- handshake readBuffer privateKey publicKeyFileOrDir tp
        case p of
          Nothing -> pure Nothing
          Just pub -> do
            sendPublicKeyFingerprint privateKey pub tp
            pure (Just pub)

    -- 强制校验身份
    case mPubKey of
      Nothing -> do
        closeTP tp
        throwIO $ userError "RSA Transport Error: Handshake failed. Identity unverified."
      Just pub ->
        return RSA { peerPublicKey = pub, .. }

  recvData (RSA {..}) _ = recvDataOaep readBuffer privateKey tp
  sendData (RSA {..}) = sendDataOaep peerPublicKey tp
  closeTP (RSA {..}) = closeTP tp
  getTPName (RSA {..}) = getTPName tp

---
--- 内部辅助函数
---

recvDataOaep :: Transport tp => TVar ByteString -> PrivateKey -> tp -> IO ByteString
recvDataOaep buf privKey tp = do
  orig <- recvEnough buf tp size
  -- decryptSafer 在 OAEP 模式下返回 IO (Either RSA.Error ByteString)
  res <- decryptSafer oaepParams privKey orig
  case res of
    Left err -> throwIO $ userError $ "RSA Decryption Failed: " ++ show err
    Right pt -> return pt

  where size = RSA.private_size privKey

sendDataOaep :: Transport tp => PublicKey -> tp -> ByteString -> IO ()
sendDataOaep peerPubKey tp bs
  | BS.null bs = pure ()
  | otherwise = do
      let (chunk, remainder) = BS.splitAt maxSize bs

      -- encrypt 在 OAEP 模式下返回 IO (Either RSA.Error ByteString)
      res <- encrypt oaepParams peerPubKey chunk
      case res of
        Left err -> throwIO $ userError $ "RSA Encryption Failed: " ++ show err
        Right ct -> do
          sendData tp ct
          unless (BS.null remainder) $ sendDataOaep peerPubKey tp remainder

  where maxSize = RSA.public_size peerPubKey - oaepSize

handshake :: Transport tp => TVar ByteString -> PrivateKey -> FilePath -> tp -> IO (Maybe PublicKey)
handshake buf privKey fileOrDir tp = do
  pubKeys <- loadPublicKeys fileOrDir
  fp <- recvDataOaep buf privKey tp
  return $ findPublicKeyByFingerprint fp pubKeys

sendPublicKeyFingerprint :: Transport tp => PrivateKey -> PublicKey -> tp -> IO ()
sendPublicKeyFingerprint privKey peerPubKey tp =
  sendDataOaep peerPubKey tp fp
  where fp = publicKeyFingerprint $ RSA.private_pub privKey

rsa :: FilePath -> FilePath -> Bool -> IO (Either String (TransportConfig tp -> TransportConfig (RSA tp)))
rsa privPath pubPath isClient = do
  ePrivKey <- readPrivateKeyPEM privPath
  return $ case ePrivKey of
    Left err      -> Left err
    Right privKey -> Right $ RSAConfig privKey pubPath isClient


numBytes n = (integerLog2 n + 7) `div` 8
integerLog2 0 = 0
integerLog2 n = 1 + integerLog2 (n `div` 2)

-- PEM 转换与密钥管理部分
pemToPrivateKey :: BS.ByteString -> Either String PrivateKey
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
readPrivateKeyPEM path = pemToPrivateKey <$> BS.readFile path

pemToPublicKeyList :: BS.ByteString -> Either String [PublicKey]
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
readPublicKeyListPEM path = pemToPublicKeyList <$> BS.readFile path

readPublicKeysFromDirectory :: FilePath -> IO (Either String [(FilePath, [PublicKey])])
readPublicKeysFromDirectory dir = do
  allFiles <- listDirectory dir
  let pemFiles = filter (".pem" `isSuffixOf`) allFiles
  let fullPaths = map (dir </>) pemFiles
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
  if isFile then fromRight [] <$> readPublicKeyListPEM publicKeyFileOrDir
            else fromRight [] <$> readAllPublicKeysFromDirectory publicKeyFileOrDir

publicKeyFingerprint :: PublicKey -> BS.ByteString
publicKeyFingerprint key = convert (hash asn1Bytes :: Digest SHA256)
  where
    asn1Bytes = publicKeyToASN1 key

findPublicKeyByFingerprint :: BS.ByteString -> [PublicKey] -> Maybe PublicKey
findPublicKeyByFingerprint fp = find (\k -> publicKeyFingerprint k == fp)

privateKeyToPEM :: PrivateKey -> BS.ByteString
privateKeyToPEM key = pemWriteBS pem
  where pem = PEM "RSA PRIVATE KEY" [] (encodeASN1' DER asn1)
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

publicKeyToASN1 :: PublicKey -> BS.ByteString
publicKeyToASN1 key = encodeASN1' DER
  [ Start Sequence
  , IntVal (RSA.public_n key)
  , IntVal (RSA.public_e key)
  , End Sequence
  ]

publicKeyToPEM :: PublicKey -> BS.ByteString
publicKeyToPEM key = pemWriteBS $ PEM "RSA PUBLIC KEY" [] (publicKeyToASN1 key)

writePrivateKeyPEM :: FilePath -> PrivateKey -> IO ()
writePrivateKeyPEM path key = BS.writeFile path (privateKeyToPEM key)

writePublicKeyPEM :: FilePath -> PublicKey -> IO ()
writePublicKeyPEM path key = BS.writeFile path (publicKeyToPEM key)

generateKeyPair :: FilePath -> Int -> IO ()
generateKeyPair prefix size = do
  (pubKey, privKey) <- generate size 65537
  writePrivateKeyPEM privFile privKey
  writePublicKeyPEM pubFile pubKey
  putStrLn $ "Keys written to " ++ privFile ++ " and " ++ pubFile
  where privFile = prefix ++ "_private_key.pem"
        pubFile  = prefix ++ "_public_key.pem"
