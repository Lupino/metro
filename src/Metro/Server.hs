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
  , ServerT
  , Servable (..)
  , getNodeEnvList
  , getServ
  , serverEnv
  , initServerEnv
  , runServerT
  , stopServerT
  , handleConn
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
import           Metro.Class                (GetPacketId, RecvPacket,
                                             Servable (..), Transport,
                                             TransportConfig)
import           Metro.Conn                 hiding (close)
import           Metro.IOHashMap            (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap            as HM (delete, elems, insertSTM,
                                                   lookupSTM)
import           Metro.Node                 (NodeEnv1, NodeMode, getNodeId,
                                             getTimer, initEnv1, runNodeT1,
                                             startNodeT, stopNodeT)
import           Metro.Session              (SessionT)
import           Metro.Utils                (getEpochTime)
import           System.Log.Logger          (infoM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

data ServerEnv serv u nid k rpkt tp = ServerEnv
  { serveServ    :: serv
  , serveState   :: TVar Bool
  , nodeEnvList  :: IOHashMap nid (NodeEnv1 u nid k rpkt tp)
  , prepare      :: SID serv -> ConnEnv tp -> IO (Maybe (nid, u))
  , gen          :: IO k
  , keepalive    :: Int64
  , defSessTout  :: Int64
  , nodeMode     :: NodeMode
  , serveName    :: String
  , mapTransport :: TransportConfig (STP serv) -> TransportConfig tp
  }


newtype ServerT serv u nid k rpkt tp m a = ServerT {unServerT :: ReaderT (ServerEnv serv u nid k rpkt tp) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ServerEnv serv u nid k rpkt tp)
    )

instance MonadTrans (ServerT serv u nid k rpkt tp) where
  lift = ServerT . lift

instance MonadUnliftIO m => MonadUnliftIO (ServerT serv u nid k rpkt tp m) where
  askUnliftIO = ServerT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runServerT r))
  withRunInIO inner = ServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runServerT r)

runServerT :: ServerEnv serv u nid k rpkt tp -> ServerT serv u nid k rpkt tp m a -> m a
runServerT sEnv = flip runReaderT sEnv . unServerT

initServerEnv
  :: (MonadIO m, Servable serv)
  => NodeMode -> String
  -> ServerConfig serv -> Int64 -> Int64 -> IO k
  -> (TransportConfig (STP serv) -> TransportConfig tp)
  -> (SID serv -> ConnEnv tp -> IO (Maybe (nid, u)))
  -> m (ServerEnv serv u nid k rpkt tp)
initServerEnv nodeMode serveName sc keepalive defSessTout gen mapTransport prepare = do
  serveServ   <- newServer sc
  serveState  <- newTVarIO True
  nodeEnvList <- newIOHashMap
  pure ServerEnv{..}

serveForever
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m ()
serveForever sess = do
  name <- asks serveName
  liftIO $ infoM "Metro.Servable" $ name ++ "Server started"
  state <- asks serveState
  void . runMaybeT . forever $ do
    e <- lift $ tryServeOnce sess
    when (isLeft e) mzero
    alive <- readTVarIO state
    unless alive mzero
  liftIO $ infoM "Metro.Servable" $ name ++ "Server closed"

tryServeOnce
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m (Either SomeException ())
tryServeOnce sess = tryAny (serveOnce sess)

serveOnce
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m ()
serveOnce sess = do
  ServerEnv {..} <- ask
  r <- servOnce serveServ
  case r of
    Nothing -> return ()
    Just (servID, stp) -> void . async $ do
      connEnv <- initConnEnv $ mapTransport stp
      mnid <- liftIO $ prepare servID connEnv
      forM_ mnid $ \(nid, uEnv) ->
        void $ handleConn "Client" servID connEnv nid uEnv sess

handleConn
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => String
  -> SID serv
  -> ConnEnv tp
  -> nid
  -> u
  -> SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m (NodeEnv1 u nid k rpkt tp)
handleConn n servID connEnv nid uEnv sess = do
    ServerEnv {..} <- ask

    liftIO $ infoM "Metro.Servable" (serveName ++ n ++ ": " ++ show nid ++ " connected")
    env0 <- initEnv1 nodeMode connEnv uEnv nid defSessTout gen

    env1 <- atomically $ do
      v <- HM.lookupSTM nodeEnvList nid
      HM.insertSTM nodeEnvList nid env0
      pure v

    mapM_ (`runNodeT1` stopNodeT) env1

    void $ async $ do
      onConnEnter serveServ servID
      lift . runNodeT1 env0 $ startNodeT sess
      onConnLeave serveServ servID
      liftIO $ infoM "Metro.Servable" (serveName ++ n ++ ": " ++ show nid ++ " disconnected")

    return env0

startServer
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => ServerEnv serv u nid k rpkt tp
  -> SessionT u nid k rpkt tp m ()
  -> m ()
startServer sEnv sess = do
  when (keepalive sEnv > 0) $ runCheckNodeState (keepalive sEnv) (nodeEnvList sEnv)
  runServerT sEnv $ serveForever sess
  liftIO $ servClose $ serveServ sEnv

stopServerT :: (MonadIO m, Servable serv) => ServerT serv u nid k rpkt tp m ()
stopServerT = do
  ServerEnv {..} <- ask
  atomically $ writeTVar serveState False
  liftIO $ servClose serveServ

runCheckNodeState
  :: (MonadUnliftIO m, Eq nid, Hashable nid, Transport tp)
  => Int64 -> IOHashMap nid (NodeEnv1 u nid k rpkt tp) -> m ()
runCheckNodeState alive envList = void . async . forever $ do
  threadDelay $ fromIntegral alive * 1000 * 1000
  mapM_ (checkAlive envList) =<< HM.elems envList

  where checkAlive
          :: (MonadUnliftIO m, Eq nid, Hashable nid, Transport tp)
          => IOHashMap nid (NodeEnv1 u nid k rpkt tp)
          -> NodeEnv1 u nid k rpkt tp -> m ()
        checkAlive ref env1 = runNodeT1 env1 $ do
              expiredAt <- (alive +) <$> getTimer
              now <- getEpochTime
              when (now > expiredAt) $ do
                nid <- getNodeId
                stopNodeT
                HM.delete ref nid

serverEnv :: Monad m => ServerT serv u nid k rpkt tp m (ServerEnv serv u nid k rpkt tp)
serverEnv = ask

getNodeEnvList :: ServerEnv serv u nid k rpkt tp -> IOHashMap nid (NodeEnv1 u nid k rpkt tp)
getNodeEnvList = nodeEnvList

getServ :: ServerEnv serv u nid k rpkt tp -> serv
getServ = serveServ
