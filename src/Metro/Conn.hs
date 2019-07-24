{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Metro.Conn
  ( ConnEnv
  , ConnT
  , FromConn (..)
  , runConnT
  , initConnEnv
  , receive
  , send
  , close
  , statusTVar
  ) where

import           Control.Monad              (when)
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (concat, drop, empty, length,
                                                  null, take)
import           Metro.Class
import qualified Metro.Lock                 as L (Lock, new, with)
import           Metro.Transport
import           UnliftIO

data ConnEnv tp = ConnEnv
  { transport :: tp
  , readLock  :: L.Lock
  , writeLock :: L.Lock
  , buffer    :: TVar ByteString
  , status    :: TVar Bool
  }

newtype ConnT tp m a = ConnT { unConnT :: ReaderT (ConnEnv tp) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadReader (ConnEnv tp)
    )

instance MonadUnliftIO m => MonadUnliftIO (ConnT tp m) where
  askUnliftIO = ConnT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runConnT r))
  withRunInIO inner = ConnT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runConnT r)

class FromConn m where
  fromConn :: Monad n => ConnT tp n a -> m tp n a

instance FromConn ConnT where
  fromConn = id

runConnT :: ConnEnv tp -> ConnT tp m a -> m a
runConnT connEnv = flip runReaderT connEnv . unConnT

initConnEnv :: (MonadIO m, Transport tp) => TransportConfig tp -> m (ConnEnv tp)
initConnEnv config = do
  readLock <- L.new
  writeLock <- L.new
  status <- newTVarIO True
  buffer <- newTVarIO B.empty
  transport <- liftIO $ newTransport config
  return ConnEnv{..}

receive :: (MonadUnliftIO m, Transport tp, Packet pkt) => ConnT tp m pkt
receive = do
  connEnv@ConnEnv{..} <- ask
  L.with readLock $ lift $ recvPacket (recv' connEnv)

recv' :: (MonadIO m, Transport tp) => ConnEnv tp -> Int -> m ByteString
recv' ConnEnv{..} nbytes = do
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
          buf <- recvData transport 1024
          when (B.null buf) $ throwIO TransportClosed
          if B.length buf >= nb then return buf
                                else do
                                  otherBuf <- readBuf (nb - B.length buf)
                                  return $! B.concat [ buf, otherBuf ]

send :: (MonadUnliftIO m, Transport tp, Packet pkt) => pkt -> ConnT tp m ()
send pkt = do
  ConnEnv{..} <- ask
  L.with writeLock $ lift $ sendPacket pkt (liftIO . sendData transport)

close :: (MonadIO m, Transport tp) => ConnT tp m ()
close = do
  ConnEnv{..} <- ask
  atomically $ writeTVar status False
  liftIO $ closeTransport transport

statusTVar :: Monad m => ConnT tp m (TVar Bool)
statusTVar = status <$> ask
