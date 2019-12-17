{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Metro.Transport.BS
  ( BSTransport
  , BSHandle
  , newBSHandle
  , feed
  , bsTransportConfig
  ) where

import           Control.Monad   (when)
import           Data.ByteString (ByteString, empty)
import qualified Data.ByteString as B (drop, take)
import           Metro.Transport
import           UnliftIO

newtype BSHandle = BSHandle (TVar ByteString)

newBSHandle :: MonadIO m => ByteString -> m BSHandle
newBSHandle bs = BSHandle <$> newTVarIO bs

feed :: MonadIO m => BSHandle -> ByteString -> m ()
feed (BSHandle h) bs = atomically $ do
  bs0 <- readTVar h
  writeTVar h $ bs0 <> bs

data BSTransport = BS
  { bsHandle :: TVar ByteString
  , bsWriter :: ByteString -> IO ()
  , bsState  :: TVar Bool
  }

instance Transport BSTransport where
  data TransportConfig BSTransport = BSConfig BSHandle (ByteString -> IO ())
  newTransport (BSConfig (BSHandle bsHandle) bsWriter) = do
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
