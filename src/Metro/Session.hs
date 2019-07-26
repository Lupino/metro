{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Metro.Session
  ( SessionEnv (..)
  , newSessionEnv
  , SessionT
  , runSessionT
  , send
  , sessionState
  , feed
  , receive
  , readerSize
  , getSessionId

  , makeResponse
  , makeResponse_
  ) where

import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Metro.Class                (Packet, PacketId, setPacketId)
import           Metro.Conn                 (ConnT, FromConn (..), statusTVar)
import qualified Metro.Conn                 as Conn (send)
import           Metro.Transport            (Transport)
import           UnliftIO

data SessionEnv k pkt = SessionEnv
  { sessionData :: TVar [Maybe pkt]
  , sessionId   :: k
  }

newSessionEnv :: MonadIO m => k -> [Maybe pkt] -> m (SessionEnv k pkt)
newSessionEnv sessionId pkts = do
  sessionData <- newTVarIO pkts
  pure SessionEnv {..}

newtype SessionT k pkt tp m a = SessionT { unSessionT :: ReaderT (SessionEnv k pkt) (ConnT tp m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (SessionEnv k pkt))

instance MonadTrans (SessionT k pkt tp) where
  lift = SessionT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (SessionT k pkt tp m) where
  askUnliftIO = SessionT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runSessionT r))
  withRunInIO inner = SessionT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runSessionT r)

instance FromConn (SessionT k pkt) where
  fromConn = SessionT . lift

runSessionT :: SessionEnv k pkt -> SessionT k pkt tp m a -> ConnT tp m a
runSessionT aEnv = flip runReaderT aEnv . unSessionT

sessionState :: MonadIO m => SessionT k pkt tp m Bool
sessionState = readTVarIO =<< fromConn statusTVar

send
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt)
  => pkt -> SessionT k pkt tp m ()
send pkt = do
  mid <- getSessionId
  fromConn $ Conn.send $ setPacketId mid pkt

feed :: (MonadIO m) => Maybe pkt -> SessionT k pkt tp m ()
feed pkt = do
  reader <- asks sessionData
  atomically . modifyTVar' reader $ \v -> v ++ [pkt]

receive :: (MonadIO m, Transport tp) => SessionT k pkt tp m (Maybe pkt)
receive = do
  reader <- asks sessionData
  st <- fromConn statusTVar
  atomically $ do
    v <- readTVar reader
    if null v then do
      s <- readTVar st
      if s then retrySTM
           else pure Nothing
    else do
      writeTVar reader $! tail v
      pure $ head v

readerSize :: MonadIO m => SessionT k pkt tp m Int
readerSize = fmap length $ readTVarIO =<< asks sessionData

getSessionId :: Monad m => SessionT k pkt tp m k
getSessionId = asks sessionId

makeResponse
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt)
  => (pkt -> m (Maybe pkt)) -> SessionT k pkt tp m ()
makeResponse f = mapM_ doSend =<< receive

  where doSend pkt = mapM_ send =<< (lift . f) pkt

makeResponse_
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt)
  => (pkt -> Maybe pkt) -> SessionT k pkt tp m ()
makeResponse_ f = makeResponse (pure . f)
