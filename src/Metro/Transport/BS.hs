{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Metro.Transport.BS
  ( BSTransport
  , BSHandle
  , newBSHandle
  , newBSHandle_
  , feed
  , bsTransportConfig
  ) where

import           Control.Monad   (when)
import           Data.ByteString (ByteString, empty)
import qualified Data.ByteString as B (drop, length, take)
import           Metro.Class     (Transport (..))
import           UnliftIO

data BSHandle = BSHandle Int (TVar ByteString)

newBSHandle :: MonadIO m => ByteString -> m BSHandle
newBSHandle = newBSHandle_ 4096

newBSHandle_ :: MonadIO m => Int -> ByteString -> m BSHandle
newBSHandle_ size bs = BSHandle size <$> newTVarIO bs

feed :: MonadIO m => BSHandle -> ByteString -> m ()
feed (BSHandle size h) bs = atomically $ do
  bs0 <- readTVar h

  when (B.length bs0 > size) retrySTM

  writeTVar h $ bs0 <> bs

data BSTransport = BS
  { bsHandle :: TVar ByteString
  , bsWriter :: ByteString -> IO ()
  , bsState  :: TVar Bool
  }

instance Transport BSTransport where
  data TransportConfig BSTransport = BSConfig BSHandle (ByteString -> IO ())
  newTransport (BSConfig (BSHandle _ bsHandle) bsWriter) = do
    bsState <- newTVarIO True
    return BS {..}
  recvData BS {..} nbytes = atomically $ do
    bs <- readTVar bsHandle
    if bs == empty then do
      status <- readTVar bsState
      if status then retrySTM
                else return bs
    else do
      writeTVar bsHandle $ B.drop nbytes bs
      return $ B.take nbytes bs
  sendData BS {..} bs = do
    status <- readTVarIO bsState
    when status $ bsWriter bs
  closeTransport BS {..} = atomically $ writeTVar bsState False

bsTransportConfig :: BSHandle -> (ByteString -> IO ()) -> TransportConfig BSTransport
bsTransportConfig = BSConfig
