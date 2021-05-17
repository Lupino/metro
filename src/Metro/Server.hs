{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Metro.Server
  ( startServer
  , startServer_
  , ServerEnv
  , ServerT
  , Servable (..)
  , getNodeEnvList
  , getServ
  , serverEnv
  , initServerEnv

  -- server env action
  , setServerName
  , setNodeMode
  , setSessionMode
  , setDefaultSessionTimeout
  , setKeepalive

  , setOnNodeLeave

  , runServerT
  , stopServerT
  , handleConn
  ) where

import           Control.Monad              (forM_, forever, unless, void, when)
import           Control.Monad.Cont         (callCC, runContT)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Either                (isLeft)
import           Data.Hashable
import           Data.IOHashMap             (IOHashMap)
import qualified Data.IOHashMap             as HM (delete, elems, empty)
import qualified Data.IOHashMap.STM         as HMS (insert, lookup)
import           Data.Int                   (Int64)
import           Metro.Class                (GetPacketId, RecvPacket,
                                             Servable (..), Transport,
                                             TransportConfig)
import           Metro.Conn                 hiding (close)
import           Metro.Node                 (NodeEnv1, NodeMode (..),
                                             SessionMode (..), getNodeId,
                                             getTimer, initEnv1, runNodeT1,
                                             startNodeT_, stopNodeT)
import qualified Metro.Node                 as Node
import           Metro.Session              (SessionT)
import           Metro.Utils                (getEpochTime)
import           System.Log.Logger          (errorM, infoM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

data ServerEnv serv u nid k rpkt tp = ServerEnv
    { serveServ   :: serv
    , serveState  :: TVar Bool
    , nodeEnvList :: IOHashMap nid (NodeEnv1 u nid k rpkt tp)
    , prepare     :: SID serv -> ConnEnv tp -> IO (Maybe (nid, u))
    , gen         :: IO k
    , keepalive   :: TVar Int64 -- client keepalive seconds
    , defSessTout :: TVar Int64 -- session timeout seconds
    , nodeMode    :: NodeMode
    , sessionMode :: SessionMode
    , serveName   :: String
    , onNodeLeave :: TVar (Maybe (nid -> u -> IO ()))
    , mapTP       :: TransportConfig (STP serv) -> TransportConfig tp
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
  withRunInIO inner = ServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runServerT r)

runServerT :: ServerEnv serv u nid k rpkt tp -> ServerT serv u nid k rpkt tp m a -> m a
runServerT sEnv = flip runReaderT sEnv . unServerT

initServerEnv
  :: (MonadIO m, Servable serv)
  => ServerConfig serv -> IO k
  -> (TransportConfig (STP serv) -> TransportConfig tp)
  -> (SID serv -> ConnEnv tp -> IO (Maybe (nid, u)))
  -> m (ServerEnv serv u nid k rpkt tp)
initServerEnv sc gen mapTP prepare = do
  serveServ   <- newServer sc
  serveState  <- newTVarIO True
  nodeEnvList <- HM.empty
  onNodeLeave <- newTVarIO Nothing
  keepalive   <- newTVarIO 300
  defSessTout <- newTVarIO 300
  pure ServerEnv
    { nodeMode    = Multi
    , sessionMode = SingleAction
    , serveName   = "Metro"
    , ..
    }

setNodeMode
  :: NodeMode -> ServerEnv serv u nid k rpkt tp -> ServerEnv serv u nid k rpkt tp
setNodeMode mode sEnv = sEnv {nodeMode = mode}

setSessionMode
  :: SessionMode -> ServerEnv serv u nid k rpkt tp -> ServerEnv serv u nid k rpkt tp
setSessionMode mode sEnv = sEnv {sessionMode = mode}

setServerName
  :: String -> ServerEnv serv u nid k rpkt tp -> ServerEnv serv u nid k rpkt tp
setServerName n sEnv = sEnv {serveName = n}

setKeepalive
  :: MonadIO m => ServerEnv serv u nid k rpkt tp -> Int -> m ()
setKeepalive sEnv =
  atomically . writeTVar (keepalive sEnv) . fromIntegral

setDefaultSessionTimeout
  :: MonadIO m => ServerEnv serv u nid k rpkt tp -> Int -> m ()
setDefaultSessionTimeout sEnv =
  atomically . writeTVar (defSessTout sEnv) . fromIntegral

setOnNodeLeave :: MonadIO m => ServerEnv serv u nid k rpkt tp -> (nid -> u -> IO ()) -> m ()
setOnNodeLeave sEnv = atomically . writeTVar (onNodeLeave sEnv) . Just

serveForever
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => (rpkt -> m Bool)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m ()
serveForever preprocess sess = do
  name <- asks serveName
  liftIO $ infoM "Metro.Server" $ name ++ "Server started"
  state <- asks serveState
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    e <- lift $ tryServeOnce preprocess sess
    when (isLeft e) $ exit ()
    alive <- readTVarIO state
    unless alive $ exit ()
  liftIO $ infoM "Metro.Server" $ name ++ "Server closed"

tryServeOnce
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => (rpkt -> m Bool)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m (Either SomeException ())
tryServeOnce preprocess sess = tryAny (serveOnce preprocess sess)

serveOnce
  :: ( MonadUnliftIO m
     , Transport tp
     , Show nid, Eq nid, Hashable nid
     , Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt
     , Servable serv)
  => (rpkt -> m Bool)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m ()
serveOnce preprocess sess = do
  ServerEnv {..} <- ask
  servOnce serveServ $ doServeOnce preprocess sess

doServeOnce
  :: ( MonadUnliftIO m
     , Transport tp
     , Show nid, Eq nid, Hashable nid
     , Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt
     , Servable serv)
  => (rpkt -> m Bool)
  -> SessionT u nid k rpkt tp m ()
  -> Maybe (SID serv, TransportConfig (STP serv))
  -> ServerT serv u nid k rpkt tp m ()
doServeOnce _ _ Nothing = return ()
doServeOnce preprocess sess (Just (servID, stp)) = do
  ServerEnv {..} <- ask
  connEnv <- initConnEnv $ mapTP stp
  mnid <- liftIO $ prepare servID connEnv
  forM_ mnid $ \(nid, uEnv) -> do
    (_, io) <- handleConn "Client" servID connEnv nid uEnv preprocess sess
    r <- waitCatch io
    case r of
      Left e  -> liftIO $ errorM "Metro.Server" $ "Handle connection error " ++ show e
      Right _ -> return ()

handleConn
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => String
  -> SID serv
  -> ConnEnv tp
  -> nid
  -> u
  -> (rpkt -> m Bool)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT serv u nid k rpkt tp m (NodeEnv1 u nid k rpkt tp, Async ())
handleConn n servID connEnv nid uEnv preprocess sess = do
    ServerEnv {..} <- ask

    connName <- liftIO $ getConnEnvName connEnv

    liftIO $ infoM "Metro.Server" (serveName ++ n ++ ": " ++ showNid nid ++ "@" ++ connName ++ " connected")
    env0 <- initEnv1
      (Node.setNodeMode nodeMode
      . Node.setSessionMode sessionMode
      . Node.setDefaultSessionTimeout defSessTout) connEnv uEnv nid gen

    env1 <- atomically $ do
      v <- HMS.lookup nid nodeEnvList
      HMS.insert nid env0 nodeEnvList
      pure v

    mapM_ (`runNodeT1` stopNodeT) env1

    io <- async $ do
      onConnEnter serveServ servID
      lift . runNodeT1 env0 $ startNodeT_ preprocess sess
      onConnLeave serveServ servID
      nodeLeave <- readTVarIO onNodeLeave
      case nodeLeave of
        Nothing -> pure ()
        Just f  -> liftIO $ f nid uEnv
      liftIO $ infoM "Metro.Server" (serveName ++ n ++ ": " ++ showNid nid ++ " disconnected")

    return (env0, io)

showNid :: Show a => a -> String
showNid = r . show
  where r :: String -> String
        r ('"':xs)  = take (length xs - 1) xs
        r ('\'':xs) = take (length xs - 1) xs
        r xs        = xs

startServer
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => ServerEnv serv u nid k rpkt tp
  -> SessionT u nid k rpkt tp m ()
  -> m ()
startServer sEnv = startServer_ sEnv (const $ return True)

startServer_
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt, Servable serv)
  => ServerEnv serv u nid k rpkt tp
  -> (rpkt -> m Bool)
  -> SessionT u nid k rpkt tp m ()
  -> m ()
startServer_ sEnv preprocess sess = do
  runCheckNodeState (keepalive sEnv) (nodeEnvList sEnv)
  runServerT sEnv $ serveForever preprocess sess
  liftIO $ servClose $ serveServ sEnv

stopServerT :: (MonadIO m, Servable serv) => ServerT serv u nid k rpkt tp m ()
stopServerT = do
  ServerEnv {..} <- ask
  atomically $ writeTVar serveState False
  liftIO $ servClose serveServ

runCheckNodeState
  :: (MonadUnliftIO m, Eq nid, Hashable nid, Transport tp)
  => TVar Int64 -> IOHashMap nid (NodeEnv1 u nid k rpkt tp) -> m ()
runCheckNodeState alive envList = void . async . forever $ do
  t <- atomically $ do
    tt <- readTVar alive
    if tt == 0 then retrySTM
               else return tt

  threadDelay $ fromIntegral t * 1000 * 1000
  mapM_ (checkAlive envList) =<< HM.elems envList

  where checkAlive
          :: (MonadUnliftIO m, Eq nid, Hashable nid, Transport tp)
          => IOHashMap nid (NodeEnv1 u nid k rpkt tp)
          -> NodeEnv1 u nid k rpkt tp -> m ()
        checkAlive ref env1 = runNodeT1 env1 $ do
              t <- readTVarIO alive
              expiredAt <- (t +) <$> getTimer
              now <- getEpochTime
              when (now > expiredAt) $ do
                nid <- getNodeId
                stopNodeT
                HM.delete nid ref

serverEnv :: Monad m => ServerT serv u nid k rpkt tp m (ServerEnv serv u nid k rpkt tp)
serverEnv = ask

getNodeEnvList :: ServerEnv serv u nid k rpkt tp -> IOHashMap nid (NodeEnv1 u nid k rpkt tp)
getNodeEnvList = nodeEnvList

getServ :: ServerEnv serv u nid k rpkt tp -> serv
getServ = serveServ
