{-# LANGUAGE RecordWildCards #-}

import           Control.Monad                        (unless, when)
import           Control.Exception                    (SomeException, try)
import           Crypto.Cipher.AES
import           Crypto.Cipher.Blowfish
import           Crypto.Cipher.Camellia
import           Crypto.Cipher.CAST5
import           Crypto.Cipher.DES
import           Crypto.Cipher.TripleDES
import           Crypto.Cipher.Twofish
import           Crypto.Cipher.Types                  (BlockCipher (..),
                                                       Cipher (..))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as B (length, null)
import qualified Data.ByteString.Char8                as C8
import           Metro.Class                          (Transport (..))
import           Metro.TP.BS                          (BSTP, makePipe)
import           Metro.TP.Crypto
import           Metro.TP.RSA
import           System.Directory                     (copyFile,
                                                       createDirectoryIfMissing,
                                                       doesDirectoryExist,
                                                       getTemporaryDirectory,
                                                       removeDirectoryRecursive)
import           System.FilePath                      ((</>))
import           System.Timeout                       (timeout)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Monadic
import           UnliftIO                             (async, cancel, waitCatch)

mkTP
  :: (Transport tp, Cipher cipher, BlockCipher cipher)
  => cipher -> String -> String -> TransportConfig tp -> IO (Crypto cipher tp)
mkTP cipher method key tpc =
  newTP $ makeCrypto cipher method key tpc

mkTPUnsafe
  :: (Transport tp, Cipher cipher, BlockCipher cipher)
  => cipher -> String -> String -> TransportConfig tp -> IO (Crypto cipher tp)
mkTPUnsafe cipher method key tpc =
  newTP $ makeCryptoUnsafe cipher method key tpc

testPipe
  :: (Transport tp, Cipher cipher, BlockCipher cipher)
  => Crypto cipher tp -> Crypto cipher tp -> ByteString -> IO Bool
testPipe pipeL pipeR bs =
  if B.null bs then return True
               else do
    sendData pipeL bs
    bs0 <- recvData pipeR $ B.length bs
    return $ bs == bs0

testCrypto
  :: (Cipher cipher, BlockCipher cipher)
  => cipher -> String -> [ByteString] -> String -> Property
testCrypto cipher method bss key = monadicIO $
  unless (null key) $ do
    (pipeLC, pipeRC) <- run $ makePipe "left" "right"

    pipeL <- run $ mkTP cipher method key pipeLC
    pipeR <- run $ mkTP cipher method key pipeRC

    r <- and <$> mapM (run . testPipe pipeL pipeR) bss
    assert r

testCrypto1
  :: (Cipher cipher, BlockCipher cipher)
  => cipher -> String -> IO ()
testCrypto1 cipher method =
  quickCheck $ withMaxSuccess 10 (testCrypto cipher method)

testEmptyKeyRejected :: IO ()
testEmptyKeyRejected = do
  (pipeLC, _) <- makePipe "left" "right"
  r <- try (mkTP (undefined :: AES128) "CBC" "" pipeLC) :: IO (Either SomeException (Crypto AES128 BSTP))
  case r of
    Left _  -> return ()
    Right _ -> ioError $ userError "Expected empty key to be rejected"

testEcbRequiresUnsafe :: IO ()
testEcbRequiresUnsafe = do
  (pipeLC, _) <- makePipe "left" "right"
  r <- try (mkTP (undefined :: AES128) "ECB" "secret" pipeLC) :: IO (Either SomeException (Crypto AES128 BSTP))
  case r of
    Left _  -> return ()
    Right _ -> ioError $ userError "Expected ECB to require makeCryptoUnsafe"

data RSAFiles = RSAFiles
  { serverPrivate :: FilePath
  , serverPublic  :: FilePath
  , clientPrivate :: FilePath
  , clientPublic  :: FilePath
  , authorizedDir :: FilePath
  , emptyAuthDir  :: FilePath
  }

setupRSAFiles :: IO RSAFiles
setupRSAFiles = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "metro-rsa-test"
      serverPrefix = dir </> "server"
      clientPrefix = dir </> "client"
      authorizedDir = dir </> "authorized"
      emptyAuthDir = dir </> "empty-authorized"
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
  createDirectoryIfMissing True authorizedDir
  createDirectoryIfMissing True emptyAuthDir
  generateKeyPair serverPrefix 1024
  generateKeyPair clientPrefix 1024
  let serverPrivate = serverPrefix ++ "_private_key.pem"
      serverPublic = serverPrefix ++ "_public_key.pem"
      clientPrivate = clientPrefix ++ "_private_key.pem"
      clientPublic = clientPrefix ++ "_public_key.pem"
  copyFile clientPublic (authorizedDir </> "client.pem")
  pure RSAFiles {..}

runRsaPair
  :: (TransportConfig BSTP -> TransportConfig (RSATP BSTP))
  -> (TransportConfig BSTP -> TransportConfig (RSATP BSTP))
  -> IO (Maybe (Either SomeException (RSATP BSTP), Either SomeException (RSATP BSTP)))
runRsaPair serverConfig clientConfig = do
  (serverPipe, clientPipe) <- makePipe "server" "client"
  serverAsync <- async $ newTP (serverConfig serverPipe)
  clientAsync <- async $ newTP (clientConfig clientPipe)
  result <- timeout 10000000 $ do
    serverResult <- waitCatch serverAsync
    clientResult <- waitCatch clientAsync
    pure (serverResult, clientResult)
  case result of
    Nothing -> do
      cancel serverAsync
      cancel clientAsync
      pure Nothing
    Just pair -> pure $ Just pair

closeRsaPair :: (Either SomeException (RSATP BSTP), Either SomeException (RSATP BSTP)) -> IO ()
closeRsaPair (serverResult, clientResult) = do
  either (const $ pure ()) closeTP serverResult
  either (const $ pure ()) closeTP clientResult

expectRsaSuccess
  :: String
  -> (TransportConfig BSTP -> TransportConfig (RSATP BSTP))
  -> (TransportConfig BSTP -> TransportConfig (RSATP BSTP))
  -> IO ()
expectRsaSuccess testName serverConfig clientConfig = do
  result <- runRsaPair serverConfig clientConfig
  case result of
    Just pair@(Right serverTp, Right clientTp) -> do
      let msg = C8.pack "rsa-aes-ok"
      sendData clientTp msg
      got <- recvData serverTp (B.length msg)
      closeRsaPair pair
      unless (got == msg) $
        ioError $ userError $ testName ++ ": RSA transport roundtrip mismatch"
    Just pair -> do
      closeRsaPair pair
      ioError $ userError $ testName ++ ": expected RSA handshake success"
    Nothing ->
      ioError $ userError $ testName ++ ": RSA handshake timed out"

expectRsaFailure
  :: String
  -> (TransportConfig BSTP -> TransportConfig (RSATP BSTP))
  -> (TransportConfig BSTP -> TransportConfig (RSATP BSTP))
  -> IO ()
expectRsaFailure testName serverConfig clientConfig = do
  result <- runRsaPair serverConfig clientConfig
  case result of
    Just pair@(Right _, Right _) -> do
      closeRsaPair pair
      ioError $ userError $ testName ++ ": expected RSA handshake failure"
    Just pair -> closeRsaPair pair
    Nothing -> pure ()

testRSATransport :: IO ()
testRSATransport = do
  files <- setupRSAFiles
  safeServer <- configServer (serverPrivate files) (authorizedDir files)
  plainServer <- configServerUnsafePlain (serverPrivate files) (authorizedDir files)
  emptyServer <- configServer (serverPrivate files) (emptyAuthDir files)
  aesClient <- configClient AES (clientPrivate files) (serverPublic files)
  plainClient <- configClient Plain (clientPrivate files) (serverPublic files)

  expectRsaSuccess "AES mode" safeServer aesClient
  expectRsaFailure "Plain mode rejected by default" safeServer plainClient
  expectRsaSuccess "Unsafe Plain mode opt-in" plainServer plainClient
  expectRsaFailure "Unauthorized client" emptyServer aesClient

safeMethods :: [String]
safeMethods = [ "CBC", "cbc", "CFB", "cfb", "CTR", "ctr" ]

unsafeMethods :: [String]
unsafeMethods = [ "ECB", "ecb" ]

main :: IO ()
main = do
  testEmptyKeyRejected
  testEcbRequiresUnsafe
  testRSATransport

  mapM_ (testCrypto1 (undefined :: AES128)) safeMethods
  mapM_ (testCrypto1 (undefined :: AES192)) safeMethods
  mapM_ (testCrypto1 (undefined :: AES256)) safeMethods

  mapM_ (testCrypto1 (undefined :: Blowfish)) safeMethods
  mapM_ (testCrypto1 (undefined :: Blowfish64)) safeMethods
  mapM_ (testCrypto1 (undefined :: Blowfish128)) safeMethods
  mapM_ (testCrypto1 (undefined :: Blowfish256)) safeMethods
  mapM_ (testCrypto1 (undefined :: Blowfish448)) safeMethods

  mapM_ (testCrypto1 (undefined :: CAST5)) safeMethods

  mapM_ (testCrypto1 (undefined :: Camellia128)) safeMethods

  mapM_ (testCrypto1 (undefined :: DES)) safeMethods

  mapM_ (testCrypto1 (undefined :: DES_EEE3)) safeMethods
  mapM_ (testCrypto1 (undefined :: DES_EDE3)) safeMethods
  mapM_ (testCrypto1 (undefined :: DES_EEE2)) safeMethods
  mapM_ (testCrypto1 (undefined :: DES_EDE2)) safeMethods

  mapM_ (testCrypto1 (undefined :: Twofish128)) safeMethods
  mapM_ (testCrypto1 (undefined :: Twofish192)) safeMethods
  mapM_ (testCrypto1 (undefined :: Twofish256)) safeMethods

  mapM_ (testCrypto1Unsafe (undefined :: AES128)) unsafeMethods

testCrypto1Unsafe
  :: (Cipher cipher, BlockCipher cipher)
  => cipher -> String -> IO ()
testCrypto1Unsafe cipher method =
  quickCheck $ withMaxSuccess 10 (testCryptoUnsafe cipher method)

testCryptoUnsafe
  :: (Cipher cipher, BlockCipher cipher)
  => cipher -> String -> [ByteString] -> String -> Property
testCryptoUnsafe cipher method bss key = monadicIO $
  unless (null key) $ do
    (pipeLC, pipeRC) <- run $ makePipe "left" "right"

    pipeL <- run $ mkTPUnsafe cipher method key pipeLC
    pipeR <- run $ mkTPUnsafe cipher method key pipeRC

    r <- and <$> mapM (run . testPipe pipeL pipeR) bss
    assert r
