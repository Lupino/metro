{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Metro.Node
  ( NodeEnv
  , NodeT
  , initEnv
  , runNodeT
  , startNodeT
  , withSessionT
  , isAlive
  , stopNodeT
  , env
  , newSessionEnv
  , request
  -- combine node env and conn env
  , NodeEnv1 (..)
  , initEnv1
  , runNodeT1

  , getTimer
  , getNodeId
  , getEpochTime
  ) where

import           Control.Monad              (forever, mzero, void)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Hashable
import           Data.Int                   (Int64)
import           Data.UnixTime              (getUnixTime, toEpochTime)
import           Foreign.C.Types            (CTime (..))
import           Metro.Class                (Packet, PacketId, getPacketId)
import           Metro.Conn                 (ConnEnv, ConnT, FromConn (..),
                                             close, receive, runConnT)
import           Metro.IOHashMap            (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap            as HM (delete, elems, insert,
                                                   lookup)
import           Metro.Session              hiding (newSessionEnv, receive,
                                             send)
import qualified Metro.Session              as S (newSessionEnv, receive, send)
import           Metro.Transport            (Transport)
import           UnliftIO


data NodeEnv u nid k pkt = NodeEnv
  { uEnv        :: u
  , nodeStatus  :: TVar Bool
  , sessionList :: IOHashMap k (SessionEnv k pkt)
  , sessionGen  :: IO k
  , nodeTimer   :: TVar Int64
  , nodeId      :: nid
  }

data NodeEnv1 u nid k pkt tp = NodeEnv1
  { nodeEnv :: NodeEnv u nid k pkt
  , connEnv :: ConnEnv tp
  }

newtype NodeT u nid k pkt tp m a = NodeT { unNodeT :: ReaderT (NodeEnv u nid k pkt) (ConnT tp m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (NodeEnv u nid k pkt)
    )

instance MonadUnliftIO m => MonadUnliftIO (NodeT u nid k pkt tp m) where
  askUnliftIO = NodeT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runNodeT r))
  withRunInIO inner = NodeT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runNodeT r)

instance MonadTrans (NodeT u nid k pkt tp) where
  lift = NodeT . lift . lift

instance FromConn (NodeT u nid k pkt) where
  fromConn = NodeT . lift

runNodeT :: NodeEnv u nid k pkt -> NodeT u nid k pkt tp m a -> ConnT tp m a
runNodeT nEnv = flip runReaderT nEnv . unNodeT

runNodeT1 :: NodeEnv1 u nid k pkt tp -> NodeT u nid k pkt tp m a -> m a
runNodeT1 NodeEnv1 {..} = runConnT connEnv . runNodeT nodeEnv

initEnv :: MonadIO m => u -> nid -> IO k -> m (NodeEnv u nid k pkt)
initEnv uEnv nodeId sessionGen = do
  nodeStatus <- newTVarIO True
  sessionList <- newIOHashMap
  nodeTimer <- newTVarIO =<< getEpochTime
  pure NodeEnv{..}

initEnv1 :: MonadIO m => ConnEnv tp -> u -> nid -> IO k -> m (NodeEnv1 u nid k pkt tp)
initEnv1 connEnv uEnv nid gen = do
  nodeEnv <- initEnv uEnv nid gen
  return NodeEnv1 {..}

runSessionT_ :: Monad m => SessionEnv k pkt -> SessionT k pkt tp m a -> NodeT u nid k pkt tp m a
runSessionT_ aEnv = fromConn . runSessionT aEnv

withSessionT :: (MonadUnliftIO m, Eq k, Hashable k) => SessionT k pkt tp m a -> NodeT u nid k pkt tp m a
withSessionT sessionT =
  bracket nextSessionId removeSession $ \sid -> do
    aEnv <- newSessionEnv_ sid
    runSessionT_ aEnv sessionT

newSessionEnv_ :: (MonadIO m, Eq k, Hashable k) => k -> NodeT u nid k pkt tp m (SessionEnv k pkt)
newSessionEnv_ sid = do
  NodeEnv{..} <- ask
  sEnv <- S.newSessionEnv sid []
  HM.insert sessionList sid sEnv
  return sEnv

newSessionEnv :: (MonadIO m, Eq k, Hashable k) => NodeT u nid k pkt tp m (SessionEnv k pkt)
newSessionEnv = newSessionEnv_ =<< nextSessionId

nextSessionId :: MonadIO m => NodeT u nid k pkt tp m k
nextSessionId = liftIO =<< asks sessionGen

removeSession :: (MonadIO m, Eq k, Hashable k) => k -> NodeT u nid k pkt tp m ()
removeSession mid = do
  ref <- asks sessionList
  HM.delete ref mid

tryMainLoop
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt, Eq k, Hashable k)
  => SessionT k pkt tp m () -> NodeT u nid k pkt tp m ()
tryMainLoop sessionHandler = do
  r <- tryAny $ mainLoop sessionHandler
  case r of
    Left _  -> stopNodeT
    Right _ -> pure ()

mainLoop
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt, Eq k, Hashable k)
  => SessionT k pkt tp m () -> NodeT u nid k pkt tp m ()
mainLoop sessionHandler = do
  NodeEnv{..} <- ask
  pkt <- fromConn receive
  setTimer =<< getEpochTime
  void . async $ tryDoFeed pkt sessionHandler

tryDoFeed
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt, Eq k, Hashable k)
  => pkt -> SessionT k pkt tp m () -> NodeT u nid k pkt tp m ()
tryDoFeed pkt sessionHandler = do
  r <- tryAny $ doFeed pkt sessionHandler
  case r of
    Left _  -> stopNodeT
    Right _ -> pure ()

doFeed
  :: (MonadIO m, Packet pkt, PacketId k pkt, Eq k, Hashable k)
  => pkt -> SessionT k pkt tp m () -> NodeT u nid k pkt tp m ()
doFeed pkt sessionHandler = do
  NodeEnv{..} <- ask
  v <- HM.lookup sessionList $ getPacketId pkt
  case v of
    Just aEnv ->
      runSessionT_ aEnv $ feed $ Just pkt
    Nothing    -> do
      let sid = getPacketId pkt
      sEnv <- S.newSessionEnv sid [Just pkt]
      runSessionT_ sEnv sessionHandler

startNodeT
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt, Eq k, Hashable k)
  => SessionT k pkt tp m () -> NodeT u nid k pkt tp m ()
startNodeT sessionHandler = do
  void . runMaybeT . forever $ do
    alive <- lift isAlive
    if alive then lift $ tryMainLoop sessionHandler
             else mzero

  doFeedError

isAlive :: MonadIO m => NodeT u nid k pkt tp m Bool
isAlive = readTVarIO =<< asks nodeStatus

doFeedError :: MonadIO m => NodeT u nid k pkt tp m ()
doFeedError =
  asks sessionList >>= HM.elems >>= mapM_ go
  where go :: MonadIO m => SessionEnv k pkt -> NodeT u nid k pkt tp m ()
        go aEnv = runSessionT_ aEnv $ feed Nothing

stopNodeT :: (MonadIO m, Transport tp) => NodeT u nid k pkt tp m ()
stopNodeT = do
  st <- asks nodeStatus
  atomically $ writeTVar st False
  fromConn close

env :: Monad m => NodeT u nid k pkt tp m u
env = asks uEnv

request
  :: (MonadUnliftIO m, Transport tp, Packet pkt, PacketId k pkt, Eq k, Hashable k)
  => pkt -> NodeT u nid k pkt tp m (Maybe pkt)
request pkt = do
  alive <- isAlive
  if alive then
    withSessionT $ do
      S.send pkt
      S.receive

  else return Nothing

getTimer :: MonadIO m => NodeT u nid k pkt tp m Int64
getTimer = readTVarIO =<< asks nodeTimer

setTimer :: MonadIO m => Int64 -> NodeT u nid k pkt tp m ()
setTimer t = do
  v <- asks nodeTimer
  atomically $ writeTVar v t

getNodeId :: Monad m => NodeT n nid k pkt tp m nid
getNodeId = asks nodeId

-- utils
getEpochTime :: MonadIO m => m Int64
getEpochTime = liftIO $ un . toEpochTime <$> getUnixTime
  where un :: CTime -> Int64
        un (CTime t) = t
