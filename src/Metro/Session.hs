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

  , isTimeout

  , makeResponse
  , makeResponse_
  ) where

import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Int                   (Int64)
import           Metro.Class                (SendPacket, SetPacketId,
                                             setPacketId)
import           Metro.Conn                 (ConnT, FromConn (..), statusTVar)
import qualified Metro.Conn                 as Conn (send)
import           Metro.Transport            (Transport)
import           Metro.Utils                (getEpochTime)
import           UnliftIO

data SessionEnv u nid k rpkt = SessionEnv
  { sessionData    :: TVar [Maybe rpkt]
  , sessionNid     :: nid
  , sessionId      :: k
  , sessionUEnv    :: u
  , sessionTimer   :: TVar Int64
  , sessionTimeout :: Int64
  }

newSessionEnv :: MonadIO m => u -> nid -> k -> Int64 -> [Maybe rpkt] -> m (SessionEnv u nid k rpkt)
newSessionEnv sessionUEnv sessionNid sessionId sessionTimeout rpkts = do
  sessionData <- newTVarIO rpkts
  sessionTimer <- newTVarIO =<< getEpochTime
  pure SessionEnv {..}

newtype SessionT u nid k rpkt tp m a = SessionT { unSessionT :: ReaderT (SessionEnv u nid k rpkt) (ConnT tp m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (SessionEnv u nid k rpkt))

instance MonadTrans (SessionT u nid k rpkt tp) where
  lift = SessionT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (SessionT u nid k rpkt tp m) where
  askUnliftIO = SessionT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runSessionT r))
  withRunInIO inner = SessionT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runSessionT r)

instance FromConn (SessionT u nid k rpkt) where
  fromConn = SessionT . lift

runSessionT :: SessionEnv u nid k rpkt -> SessionT u nid k rpkt tp m a -> ConnT tp m a
runSessionT aEnv = flip runReaderT aEnv . unSessionT

sessionState :: MonadIO m => SessionT u nid k rpkt tp m Bool
sessionState = readTVarIO =<< fromConn statusTVar

send
  :: (MonadUnliftIO m, Transport tp, SendPacket spkt, SetPacketId k spkt)
  => spkt -> SessionT u nid k rpkt tp m ()
send rpkt = do
  mid <- getSessionId
  fromConn $ Conn.send $ setPacketId mid rpkt

feed :: (MonadIO m) => Maybe rpkt -> SessionT u nid k rpkt tp m ()
feed rpkt = do
  reader <- asks sessionData
  setTimer =<< getEpochTime
  atomically . modifyTVar' reader $ \v -> v ++ [rpkt]

receive :: (MonadIO m, Transport tp) => SessionT u nid k rpkt tp m (Maybe rpkt)
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

readerSize :: MonadIO m => SessionT u nid k rpkt tp m Int
readerSize = fmap length $ readTVarIO =<< asks sessionData

getSessionId :: Monad m => SessionT u nid k rpkt tp m k
getSessionId = asks sessionId

getNodeId :: Monad m => SessionT u nid k rpkt tp m nid
getNodeId = asks sessionNid

env :: Monad m => SessionT u nid k rpkt tp m u
env = asks sessionUEnv

-- makeResponse if Nothing ignore
makeResponse
  :: (MonadUnliftIO m, Transport tp, SendPacket spkt, SetPacketId k spkt)
  => (rpkt -> m (Maybe spkt)) -> SessionT u nid k rpkt tp m ()
makeResponse f = mapM_ doSend =<< receive

  where doSend spkt = mapM_ send =<< (lift . f) spkt

makeResponse_
  :: (MonadUnliftIO m, Transport tp, SendPacket spkt, SetPacketId k spkt)
  => (rpkt -> Maybe spkt) -> SessionT u nid k rpkt tp m ()
makeResponse_ f = makeResponse (pure . f)

getTimer :: MonadIO m => SessionT u nid k rpkt tp m Int64
getTimer = readTVarIO =<< asks sessionTimer

setTimer :: MonadIO m => Int64 -> SessionT u nid k rpkt tp m ()
setTimer t = do
  v <- asks sessionTimer
  atomically $ writeTVar v t

isTimeout :: MonadIO m => SessionT u nid k rpkt tp m Bool
isTimeout = do
  t <- getTimer
  tout <- asks sessionTimeout
  now <- getEpochTime
  return $ (t + tout) < now
