module Metro.Utils
  ( getEpochTime
  , setupLog
  , recvEnough
  , foreverExit
  , lift
  ) where


import           Control.Monad             (forever, when)
import           Control.Monad.Cont        (ContT, callCC, runContT)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (concat, empty, length, null,
                                                 splitAt)
import           Data.Int                  (Int64)
import           Data.UnixTime             (getUnixTime, toEpochTime)
import           Foreign.C.Types           (CTime (..))
import           Metro.Class               (Transport (..), TransportError (..))
import           System.IO                 (stderr)
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
  if nbytes <= 0 then return B.empty
  else do
    (buf, need) <- atomically $ do
      bf <- readTVar buffer
      let (out, rest) = B.splitAt nbytes bf
      writeTVar buffer $! rest
      return $! (out, nbytes - B.length out)
    if need == 0 then return buf
                 else do
                   (chunks, overflow) <- liftIO $ readBuf [] need
                   atomically . writeTVar buffer $! overflow
                   return $! B.concat (buf : chunks)

  where readBuf :: [ByteString] -> Int -> IO ([ByteString], ByteString)
        readBuf chunks 0  = return (reverse chunks, B.empty)
        readBuf chunks nb = do
          buf <- recvData tp $ max 8192 nb -- 8k
          when (B.null buf) $ throwIO TransportClosed
          let (out, rest) = B.splitAt nb buf
              chunks' = out : chunks
          if B.length buf >= nb then return (reverse chunks', rest)
                                else readBuf chunks' (nb - B.length buf)

foreverExit :: Applicative m => ((r -> ContT r m ()) -> ContT r m ()) -> m r
foreverExit io = (`runContT` pure) $ callCC $ \exit -> forever $ io exit
