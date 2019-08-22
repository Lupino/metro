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
  , getNodeId
  , env

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

data SessionEnv u nid k pkt = SessionEnv
  { sessionData :: TVar [Maybe pkt]
  , sessionNid  :: nid
  , sessionId   :: k
  , sessionUEnv :: u
  }

newSessionEnv :: MonadIO m => u -> nid -> k -> [Maybe pkt] -> m (SessionEnv u nid k pkt)
newSessionEnv sessionUEnv sessionNid sessionId pkts = do
  sessionData <- newTVarIO pkts
  pure SessionEnv {..}

newtype SessionT u nid k pkt tp m a = SessionT { unSessionT :: ReaderT (SessionEnv u nid k pkt) (ConnT tp m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (SessionEnv u nid k pkt))

instance MonadTrans (SessionT u nid k pkt tp) where
  lift = SessionT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (SessionT u nid k pkt tp m) where
  askUnliftIO = SessionT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runSessionT r))
  withRunInIO inner = SessionT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runSessionT r)

instance FromConn (SessionT u nid k pkt) where
  fromConn = SessionT . lift

runSessionT :: SessionEnv u nid k pkt -> SessionT u nid k pkt tp m a -> ConnT tp m a
runSessionT aEnv = flip runReaderT aEnv . unSessionT

sessionState :: MonadIO m => SessionT u nid k pkt tp m Bool
sessionState = readTVarIO =<< fromConn statusTVar

send
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt)
  => pkt -> SessionT u nid k pkt tp m ()
send pkt = do
  mid <- getSessionId
  fromConn $ Conn.send $ setPacketId mid pkt

feed :: (MonadIO m) => Maybe pkt -> SessionT u nid k pkt tp m ()
feed pkt = do
  reader <- asks sessionData
  atomically . modifyTVar' reader $ \v -> v ++ [pkt]

receive :: (MonadIO m, Transport tp) => SessionT u nid k pkt tp m (Maybe pkt)
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

readerSize :: MonadIO m => SessionT u nid k pkt tp m Int
readerSize = fmap length $ readTVarIO =<< asks sessionData

getSessionId :: Monad m => SessionT u nid k pkt tp m k
getSessionId = asks sessionId

getNodeId :: Monad m => SessionT u nid k pkt tp m nid
getNodeId = asks sessionNid

env :: Monad m => SessionT u nid k pkt tp m u
env = asks sessionUEnv

-- makeResponse if Nothing ignore
makeResponse
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt)
  => (pkt -> m (Maybe pkt)) -> SessionT u nid k pkt tp m ()
makeResponse f = mapM_ doSend =<< receive

  where doSend pkt = mapM_ send =<< (lift . f) pkt

makeResponse_
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt)
  => (pkt -> Maybe pkt) -> SessionT u nid k pkt tp m ()
makeResponse_ f = makeResponse (pure . f)
