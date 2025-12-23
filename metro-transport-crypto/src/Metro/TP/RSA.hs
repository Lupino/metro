{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Metro.TP.RSA
  ( RSA
  , rsa
  ) where

import           Control.Monad            (void, when)
import           Crypto.Hash              (Digest, SHA256 (..), hash)
import           Crypto.PubKey.RSA        (PrivateKey, PublicKey)
import           Crypto.PubKey.RSA.PKCS15 (decryptSafer, encrypt)
import qualified Crypto.PubKey.RSA.Types  as RSA
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types
import           Data.ByteArray           (convert)
import           Data.ByteString          (ByteString, empty)
import qualified Data.ByteString          as BS
import           Data.Either              (fromRight)
import           Data.List                (find, isSuffixOf)
import           Data.Maybe               (listToMaybe)
import           Data.PEM                 (PEM (..), pemContent, pemParseBS)
import           Metro.Class              (Transport (..))
import           Metro.Utils              (recvEnough)
import           System.Directory         (doesFileExist, listDirectory)
import           System.FilePath          ((</>))
import           UnliftIO


-- openssl genrsa -out key.pem 2048
-- openssl rsa -in key.pem -pubout -out pub.pem
-- openssl rsa -in key.pem -text -noout


data RSA tp = RSA
  { readBuffer :: TVar ByteString
  , privateKey :: PrivateKey
  , mPublicKey :: Maybe PublicKey
  , tp         :: tp
  }


instance (Transport tp) => Transport (RSA tp) where
  data TransportConfig (RSA tp) = RSAConfig PrivateKey FilePath Bool (TransportConfig tp)
  newTP (RSAConfig privateKey publicKeyFileOrDir False config) = do -- server
    readBuffer <- newTVarIO empty
    tp <- newTP config
    mPublicKey <- handshake readBuffer privateKey publicKeyFileOrDir tp
    case mPublicKey of
      Nothing -> pure ()
      Just x  -> sendPublicKeyFingerprint privateKey x tp

    return RSA {..}

  newTP (RSAConfig privateKey publicKeyFileOrDir True config) = do -- client
    readBuffer <- newTVarIO empty
    tp <- newTP config
    mPublicKey <- listToMaybe <$> loadPublicKeys publicKeyFileOrDir
    case mPublicKey of
      Nothing -> pure ()
      Just x  -> do
        sendPublicKeyFingerprint privateKey x tp
        void $ handshake readBuffer privateKey publicKeyFileOrDir tp
    return RSA {..}

  recvData (RSA {..}) _ = do
    orig <- recvEnough readBuffer tp (RSA.private_size privateKey)
    fromRight empty <$> decryptSafer privateKey orig

  sendData (RSA {..}) "" = pure ()
  sendData (RSA {..}) bs = do
    case mPublicKey of
      Nothing -> closeTP tp
      Just x -> do
        msg <- fromRight empty <$> encrypt x (BS.take maxSize bs)
        sendData tp msg

        when (BS.drop maxSize bs == empty) $ sendData (RSA {..}) $ BS.drop maxSize bs


    where maxSize = RSA.private_size privateKey - 11

  closeTP (RSA {..}) = closeTP tp
  getTPName (RSA {..}) = getTPName tp


rsa :: FilePath -> FilePath -> Bool -> IO (Either String (TransportConfig tp -> TransportConfig (RSA tp)))
rsa privPath pubPath isClient = do
  ePrivKey <- readPrivateKeyPEM privPath
  return $ case ePrivKey of
             Left err      -> Left err
             Right privKey -> Right $ RSAConfig privKey pubPath isClient


-- Read RSA Private Key from PEM format
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
        (Start Sequence : IntVal _version : IntVal n : IntVal e : IntVal d :
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
  where
    numBytes n = (integerLog2 n + 8) `div` 8
    integerLog2 0 = 0
    integerLog2 n = 1 + integerLog2 (n `div` 2)

-- Read keys from files
readPrivateKeyPEM :: FilePath -> IO (Either String PrivateKey)
readPrivateKeyPEM path = pemToPrivateKey <$> BS.readFile path

-- Read multiple RSA Public Keys from PEM format
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

    numBytes n = (integerLog2 n + 8) `div` 8
    integerLog2 0 = 0
    integerLog2 n = 1 + integerLog2 (n `div` 2)


-- Read multiple public keys from a file
readPublicKeyListPEM :: FilePath -> IO (Either String [PublicKey])
readPublicKeyListPEM path = pemToPublicKeyList <$> BS.readFile path

-- Read all public keys from all .pem files in a directory
readPublicKeysFromDirectory :: FilePath -> IO (Either String [(FilePath, [PublicKey])])
readPublicKeysFromDirectory dir = do
    allFiles <- listDirectory dir
    let pemFiles = filter (\f -> ".pem" `isSuffixOf` f) allFiles
    let fullPaths = map (dir </>) pemFiles

    results <- mapM readFileWithKeys fullPaths
    return $ sequence results
  where
    readFileWithKeys path = do
        keysResult <- readPublicKeyListPEM path
        return $ case keysResult of
            Right keys -> Right (path, keys)
            Left err   -> Left (path ++ ": " ++ err)

-- Flatten all keys from directory (ignoring filenames)
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


-- Get a fingerprint (hash) of a public key for identification
publicKeyFingerprint :: PublicKey -> BS.ByteString
publicKeyFingerprint key = convert (hash asn1Bytes :: Digest SHA256)
  where
    asn1Bytes = encodeASN1' DER asn1
    asn1 = [Start Sequence, IntVal (RSA.public_n key), IntVal (RSA.public_e key), End Sequence]


-- Search for a public key by fingerprint
findPublicKeyByFingerprint :: BS.ByteString -> [PublicKey] -> Maybe PublicKey
findPublicKeyByFingerprint fp = find (\k -> publicKeyFingerprint k == fp)


handshake :: Transport tp => TVar ByteString -> PrivateKey -> FilePath -> tp -> IO (Maybe PublicKey)
handshake buf privKey fileOrDir tp = do
  challengeMsg <- recvEnough buf tp (RSA.private_size privKey)
  pubKeys <- loadPublicKeys fileOrDir
  efp <- decryptSafer privKey challengeMsg
  case efp of
    Left _   -> return Nothing
    Right fp -> return $ findPublicKeyByFingerprint fp pubKeys


sendPublicKeyFingerprint :: Transport tp => PrivateKey -> PublicKey -> tp -> IO ()
sendPublicKeyFingerprint privKey pubKey tp = do
  eChallengeMsg <- encrypt pubKey (publicKeyFingerprint $ RSA.private_pub privKey)
  case eChallengeMsg of
    Left _   -> pure ()
    Right bs -> sendData tp bs
