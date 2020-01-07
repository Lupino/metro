{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Metro.TP.BS
  ( BSTransport
  , BSHandle
  , newBSHandle
  , newBSHandle_
  , feed
  , closeBSHandle
  , bsTransportConfig

  , makePipe
  ) where

import           Control.Monad   (when)
import           Data.ByteString (ByteString, empty)
import qualified Data.ByteString as B (drop, length, take)
import           Metro.Class     (Transport (..))
import           UnliftIO

data BSHandle = BSHandle Int (TVar Bool) (TVar ByteString)

newBSHandle :: MonadIO m => ByteString -> m BSHandle
newBSHandle = newBSHandle_ 41943040 -- 40M

newBSHandle_ :: MonadIO m => Int -> ByteString -> m BSHandle
newBSHandle_ size bs = do
  state <- newTVarIO True
  BSHandle size state <$> newTVarIO bs

feed :: MonadIO m => BSHandle -> ByteString -> m ()
feed (BSHandle size state h) bs = atomically $ do
  st <- readTVar state
  when st $ do
    bs0 <- readTVar h
    when (B.length bs0 > size) retrySTM
    writeTVar h $ bs0 <> bs

closeBSHandle :: MonadIO m => BSHandle -> m ()
closeBSHandle (BSHandle _ state _) = atomically $ writeTVar state False

data BSTransport = BS
  { bsHandle :: TVar ByteString
  , bsWriter :: ByteString -> IO ()
  , bsState  :: TVar Bool
  }

instance Transport BSTransport where
  data TransportConfig BSTransport = BSConfig BSHandle (ByteString -> IO ())
  newTransport (BSConfig (BSHandle _ bsState bsHandle) bsWriter) =
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

makePipe :: MonadIO m => m (TransportConfig BSTransport, TransportConfig BSTransport)
makePipe = do
  rHandle <- newBSHandle empty
  wHandle <- newBSHandle empty

  return (bsTransportConfig rHandle (feed wHandle), bsTransportConfig wHandle (feed rHandle))
