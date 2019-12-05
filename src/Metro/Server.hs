{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Metro.Server
  ( startServer
  , ServerEnv
  , nodeEnvList
  , initServerEnv
  ) where

import           Control.Monad              (forM_, forever, mzero, unless,
                                             void, when)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Either                (isLeft)
import           Data.Hashable
import           Data.Int                   (Int64)
import           Metro.Class                (PacketId (..), RecvPacket (..),
                                             SendPacket (..))
import           Metro.Conn
import           Metro.IOHashMap            (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap            as HM (delete, elems, insertSTM,
                                                   lookupSTM)
import           Metro.Node                 (NodeEnv1, getNodeId, getTimer,
                                             initEnv1, runNodeT1, startNodeT,
                                             stopNodeT)
import           Metro.Session              (SessionT)
import           Metro.Socket               (listen)
import           Metro.Transport            (Transport, TransportConfig)
import           Metro.Utils                (getEpochTime)
import           Network.Socket             (Socket, SocketOption (KeepAlive),
                                             accept, setSocketOption)
import qualified Network.Socket             as Socket (close)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

data ServerEnv u nid k pkt tp = ServerEnv
  { serveSock   :: Socket
  , serveState  :: TVar Bool
  , nodeEnvList :: IOHashMap nid (NodeEnv1 u nid k pkt tp)
  , prepare     :: Socket -> ConnEnv tp -> IO (Maybe (nid, u))
  , gen         :: IO k
  , keepalive   :: Int64
  , defSessTout :: Int64
  }


newtype ServerT u nid k pkt tp m a = ServerT {unServerT :: ReaderT (ServerEnv u nid k pkt tp) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ServerEnv u nid k pkt tp)
    )

instance MonadTrans (ServerT u nid k pkt tp) where
  lift = ServerT . lift

instance MonadUnliftIO m => MonadUnliftIO (ServerT u nid k pkt tp m) where
  askUnliftIO = ServerT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runServerT r))
  withRunInIO inner = ServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runServerT r)

runServerT :: ServerEnv u nid k pkt tp -> ServerT u nid k pkt tp m a -> m a
runServerT sEnv = flip runReaderT sEnv . unServerT

initServerEnv
  :: MonadIO m
  => String -> Int64 -> Int64 -> IO k
  -> (Socket -> ConnEnv tp -> IO (Maybe (nid, u)))
  -> m (ServerEnv u nid k pkt tp)
initServerEnv hostPort keepalive defSessTout gen prepare = do
  serveSock <- liftIO $ listen hostPort
  serveState <- newTVarIO True
  nodeEnvList <- newIOHashMap
  pure ServerEnv{..}

serveForever
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, PacketId k pkt, RecvPacket pkt, SendPacket pkt)
  => (Socket -> TransportConfig tp)
  -> SessionT u nid k pkt tp m ()
  -> ServerT u nid k pkt tp m ()
serveForever mk sess = do
  state <- asks serveState
  void . runMaybeT . forever $ do
    e <- lift $ tryServeOnce mk sess
    when (isLeft e) mzero
    alive <- readTVarIO state
    unless alive mzero

tryServeOnce
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, PacketId k pkt, RecvPacket pkt, SendPacket pkt)
  => (Socket -> TransportConfig tp)
  -> SessionT u nid k pkt tp m ()
  -> ServerT u nid k pkt tp m (Either SomeException ())
tryServeOnce mk sess = tryAny (serveOnce mk sess)

serveOnce
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, PacketId k pkt, RecvPacket pkt, SendPacket pkt)
  => (Socket -> TransportConfig tp)
  -> SessionT u nid k pkt tp m ()
  -> ServerT u nid k pkt tp m ()
serveOnce mk sess = do
  (sock, _) <- liftIO . accept =<< asks serveSock
  liftIO $ setSocketOption sock KeepAlive 1
  void $ async $ handleConn sock (mk sock) sess

handleConn
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, PacketId k pkt, RecvPacket pkt, SendPacket pkt)
  => Socket
  -> TransportConfig tp
  -> SessionT u nid k pkt tp m ()
  -> ServerT u nid k pkt tp m ()
handleConn sock tpconfig sess = do
  ServerEnv {..} <- ask
  connEnv <- initConnEnv tpconfig

  mnid <- liftIO $ prepare sock connEnv

  forM_ mnid $ \(nid, uEnv) -> do
    env0 <- initEnv1 connEnv uEnv nid defSessTout gen

    env1 <- atomically $ do
      v <- HM.lookupSTM nodeEnvList nid
      HM.insertSTM nodeEnvList nid env0
      pure v

    mapM_ (`runNodeT1` stopNodeT) env1

    lift . runNodeT1 env0 $ startNodeT sess

startServer
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, PacketId k pkt, RecvPacket pkt, SendPacket pkt)
  => ServerEnv u nid k pkt tp
  -> (Socket -> TransportConfig tp)
  -> SessionT u nid k pkt tp m ()
  -> m ()
startServer sEnv mk sess = do
  when (keepalive sEnv > 0) $ runCheckNodeState (keepalive sEnv) (nodeEnvList sEnv)
  runServerT sEnv $ serveForever mk sess
  liftIO $ Socket.close $ serveSock sEnv

runCheckNodeState
  :: (MonadUnliftIO m, Eq nid, Hashable nid, Transport tp)
  => Int64 -> IOHashMap nid (NodeEnv1 u nid k pkt tp) -> m ()
runCheckNodeState alive envList = void . async . forever $ do
  threadDelay $ fromIntegral alive * 1000 * 1000
  mapM_ (checkAlive envList) =<< HM.elems envList

  where checkAlive
          :: (MonadUnliftIO m, Eq nid, Hashable nid, Transport tp)
          => IOHashMap nid (NodeEnv1 u nid k pkt tp)
          -> NodeEnv1 u nid k pkt tp -> m ()
        checkAlive ref env1 = runNodeT1 env1 $ do
              expiredAt <- (alive +) <$> getTimer
              now <- getEpochTime
              when (now > expiredAt) $ do
                nid <- getNodeId
                stopNodeT
                HM.delete ref nid
