module Metro.Utils
  ( getEpochTime
  ) where

import           Data.Int        (Int64)
import           Data.UnixTime   (getUnixTime, toEpochTime)
import           Foreign.C.Types (CTime (..))
import           UnliftIO

-- utils
getEpochTime :: MonadIO m => m Int64
getEpochTime = liftIO $ un . toEpochTime <$> getUnixTime
  where un :: CTime -> Int64
        un (CTime t) = t
