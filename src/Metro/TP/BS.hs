{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Metro.TP.BS
  ( BSTP
  , BSHandle
  , newBSHandle
  , newBSHandle_
  , feed
  , closeBSHandle
  , bsTPConfig

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
  state <- newTVarIO False
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

data BSTP = BS
    { bsHandle :: TVar ByteString
    , bsWriter :: ByteString -> IO ()
    , bsState  :: TVar Bool
    , bsName   :: String
    }

instance Transport BSTP where
  data TransportConfig BSTP = BSConfig BSHandle (ByteString -> IO ()) String
  newTP (BSConfig (BSHandle _ bsState bsHandle) bsWriter bsName) = do
    atomically $ writeTVar bsState True
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
  closeTP BS {..} = atomically $ do
    writeTVar bsState False
    writeTVar bsHandle empty
  getTPName BS {..} = pure bsName

bsTPConfig :: BSHandle -> (ByteString -> IO ()) -> String -> TransportConfig BSTP
bsTPConfig = BSConfig

makePipe :: MonadIO m => String -> String -> m (TransportConfig BSTP, TransportConfig BSTP)
makePipe rName wName = do
  rHandle <- newBSHandle empty
  wHandle <- newBSHandle empty

  return (bsTPConfig rHandle (feed wHandle) rName, bsTPConfig wHandle (feed rHandle) wName)
