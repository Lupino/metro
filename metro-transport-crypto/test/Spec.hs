import           Control.Monad                        (when)
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
import           Metro.Class                          (Transport (..))
import           Metro.TP.BS                          (makePipe)
import           Metro.TP.Crypto
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Monadic

mkTP
  :: (Transport tp, Cipher cipher, BlockCipher cipher)
  => cipher -> String -> String -> TransportConfig tp -> IO (Crypto cipher tp)
mkTP cipher method key tpc =
  newTransport $ makeCrypto cipher method key tpc

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
testCrypto cipher method bss key = monadicIO $ do
  when (not $ null key) $ do
    (pipeLC, pipeRC) <- run makePipe

    pipeL <- run $ mkTP cipher method key pipeLC
    pipeR <- run $ mkTP cipher method key pipeRC

    r <- all id <$> mapM (run . testPipe pipeL pipeR) bss
    assert r

testCrypto1
  :: (Cipher cipher, BlockCipher cipher)
  => cipher -> String -> IO ()
testCrypto1 cipher method =
  quickCheck $ withMaxSuccess 10 (testCrypto cipher method)

methods :: [String]
methods = [ "CBC", "cbc", "CFB", "cfb", "ECB", "ecb", "CTR", "ctr" ]

main :: IO ()
main = do
  mapM_ (testCrypto1 (undefined :: AES128)) methods
  mapM_ (testCrypto1 (undefined :: AES192)) methods
  mapM_ (testCrypto1 (undefined :: AES256)) methods

  mapM_ (testCrypto1 (undefined :: Blowfish)) methods
  mapM_ (testCrypto1 (undefined :: Blowfish64)) methods
  mapM_ (testCrypto1 (undefined :: Blowfish128)) methods
  mapM_ (testCrypto1 (undefined :: Blowfish256)) methods
  mapM_ (testCrypto1 (undefined :: Blowfish448)) methods

  mapM_ (testCrypto1 (undefined :: CAST5)) methods

  mapM_ (testCrypto1 (undefined :: Camellia128)) methods

  mapM_ (testCrypto1 (undefined :: DES)) methods

  mapM_ (testCrypto1 (undefined :: DES_EEE3)) methods
  mapM_ (testCrypto1 (undefined :: DES_EDE3)) methods
  mapM_ (testCrypto1 (undefined :: DES_EEE2)) methods
  mapM_ (testCrypto1 (undefined :: DES_EDE2)) methods

  mapM_ (testCrypto1 (undefined :: Twofish128)) methods
  mapM_ (testCrypto1 (undefined :: Twofish192)) methods
  mapM_ (testCrypto1 (undefined :: Twofish256)) methods
