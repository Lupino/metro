module Metro.Utils
  ( getEpochTime
  , setupLog
  , recvEnough
  ) where


import           Control.Monad             (when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (concat, drop, empty, length,
                                                 null, take)
import           Data.Int                  (Int64)
import           Data.UnixTime             (getUnixTime, toEpochTime)
import           Foreign.C.Types           (CTime (..))
import           Metro.Class               (Transport (..), TransportError (..))
import           System.IO                 (stderr)
import           System.Log                (Priority (..))
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           UnliftIO                  (MonadIO (..), TVar, atomically,
                                            readTVar, throwIO, writeTVar)

-- utils
getEpochTime :: MonadIO m => m Int64
getEpochTime = liftIO $ un . toEpochTime <$> getUnixTime
  where un :: CTime -> Int64
        un (CTime t) = t

setupLog :: Priority -> IO ()
setupLog logLevel = do
  removeAllHandlers
  handle <- streamHandler stderr logLevel >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (addHandler handle . setLevel logLevel)

recvEnough :: (MonadIO m, Transport tp) => TVar ByteString -> tp -> Int -> m ByteString
recvEnough buffer tp nbytes = do
  buf <- atomically $ do
    bf <- readTVar buffer
    writeTVar buffer $! B.drop nbytes bf
    return $! B.take nbytes bf
  if B.length buf == nbytes then return buf
                            else do
                              otherBuf <- liftIO $ readBuf (nbytes - B.length buf)
                              let out = B.concat [ buf, otherBuf ]
                              atomically . writeTVar buffer $! B.drop nbytes out
                              return $! B.take nbytes out

  where readBuf :: Int -> IO ByteString
        readBuf 0  = return B.empty
        readBuf nb = do
          buf <- recvData tp $ max 4096 nb -- 4k
          when (B.null buf) $ throwIO TransportClosed
          if B.length buf >= nb then return buf
                                else do
                                  otherBuf <- readBuf (nb - B.length buf)
                                  return $! B.concat [ buf, otherBuf ]
