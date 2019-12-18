module Metro.Utils
  ( getEpochTime
  , setupLog
  ) where

import           Data.Int                  (Int64)
import           Data.UnixTime             (getUnixTime, toEpochTime)
import           Foreign.C.Types           (CTime (..))
import           System.IO                 (stderr)
import           System.Log                (Priority (..))
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           UnliftIO                  (MonadIO (..))

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
