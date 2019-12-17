{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Metro.UDP
  ( startServer
  , ServerEnv
  , nodeEnvList
  , initServerEnv
  , runServerT
  , newClient
  ) where

import           Control.Monad              (forM_, forever, mzero, unless,
                                             void, when)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.ByteString            (ByteString, empty)
import           Data.Either                (isLeft)
import           Data.Hashable
import           Data.Int                   (Int64)
import           Data.Maybe                 (listToMaybe)
import           Metro.Class                (GetPacketId, RecvPacket)
import           Metro.Conn
import           Metro.IOHashMap            (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap            as HM (delete, elems, insert,
                                                   insertSTM, lookup, lookupSTM)
import           Metro.Node                 (NodeEnv1, NodeMode, getNodeId,
                                             getTimer, initEnv1, runNodeT1,
                                             startNodeT, stopNodeT)
import           Metro.Session              (SessionT)
import           Metro.Transport            (Transport, TransportConfig)
import           Metro.Transport.BS         (BSHandle, BSTransport,
                                             bsTransportConfig, feed,
                                             newBSHandle)
import           Metro.Utils                (getEpochTime)
import           Network.Socket             (AddrInfo (..), AddrInfoFlag (..),
                                             SockAddr, Socket,
                                             SocketOption (ReuseAddr),
                                             SocketType (..), addrAddress,
                                             addrFlags, addrSocketType, bind,
                                             defaultHints, getAddrInfo,
                                             setSocketOption, socket)
import qualified Network.Socket             as Socket (close)
import           Network.Socket.ByteString  (recvFrom, sendAllTo)
import           System.Log.Logger          (errorM, infoM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

data ServerEnv u nid k rpkt tp = ServerEnv
  { serveSock    :: Socket
  , serveState   :: TVar Bool
  , nodeEnvList  :: IOHashMap nid (NodeEnv1 u nid k rpkt tp)
  , bsHandleList :: IOHashMap String BSHandle
  , prepare      :: SockAddr -> ConnEnv tp -> IO (Maybe (nid, u))
  , gen          :: IO k
  , keepalive    :: Int64
  , defSessTout  :: Int64
  , nodeMode     :: NodeMode
  , serveName    :: String
  }


newtype ServerT u nid k rpkt tp m a = ServerT {unServerT :: ReaderT (ServerEnv u nid k rpkt tp) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ServerEnv u nid k rpkt tp)
    )

instance MonadTrans (ServerT u nid k rpkt tp) where
  lift = ServerT . lift

instance MonadUnliftIO m => MonadUnliftIO (ServerT u nid k rpkt tp m) where
  askUnliftIO = ServerT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runServerT r))
  withRunInIO inner = ServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runServerT r)

runServerT :: ServerEnv u nid k rpkt tp -> ServerT u nid k rpkt tp m a -> m a
runServerT sEnv = flip runReaderT sEnv . unServerT

initServerEnv
  :: MonadIO m
  => NodeMode -> String
  -> String -> Int64 -> Int64 -> IO k
  -> (SockAddr -> ConnEnv tp -> IO (Maybe (nid, u)))
  -> m (ServerEnv u nid k rpkt tp)
initServerEnv nodeMode serveName hostPort keepalive defSessTout gen prepare = do
  serveSock <- liftIO $ bindTo hostPort
  serveState <- newTVarIO True
  nodeEnvList <- newIOHashMap
  bsHandleList <- newIOHashMap
  pure ServerEnv{..}

-- Returns the first action from a list which does not throw an exception.
-- If all the actions throw exceptions (and the list of actions is not empty),
-- the last exception is thrown.
-- The operations are run outside of the catchIO cleanup handler because
-- catchIO masks asynchronous exceptions in the cleanup handler.
-- In the case of complete failure, the last exception is actually thrown.
firstSuccessful :: [IO a] -> IO a
firstSuccessful = go Nothing
  where
  -- Attempt the next operation, remember exception on failure
  go _ (p:ps) =
    do r <- tryIO p
       case r of
         Right x -> return x
         Left  e -> go (Just e) ps

  -- All operations failed, throw error if one exists
  go Nothing  [] = error "firstSuccessful: empty list"
  go (Just e) [] = throwIO e

getDatagramAddrList :: String -> IO [AddrInfo]
getDatagramAddrList hostPort = getAddrInfo (Just hints) host port
  where hints = defaultHints
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Datagram
          }

        host = getHost hostPort
        port = getService hostPort

getDatagramAddr :: String -> IO (Maybe AddrInfo)
getDatagramAddr hostPort = listToMaybe <$> getDatagramAddrList hostPort

bindTo :: String -> IO Socket
bindTo hostPort = do
  addrs <- getDatagramAddrList hostPort
  firstSuccessful $ map tryToConnect addrs
  where
  tryToConnect addr =
    bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        Socket.close  -- only done if there's an error
        (\sock -> do
          setSocketOption sock ReuseAddr 1
          bind sock $ addrAddress addr
          return sock
        )

serveForever
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => (TransportConfig BSTransport -> TransportConfig tp)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT u nid k rpkt tp m ()
serveForever mk sess = do
  name <- asks serveName
  liftIO $ infoM "Metro.UDP" $ name ++ "Server started"
  state <- asks serveState
  void . runMaybeT . forever $ do
    e <- lift $ tryServeOnce mk sess
    when (isLeft e) mzero
    alive <- readTVarIO state
    unless alive mzero
  liftIO $ infoM "Metro.UDP" $ name ++ "Server closed"

tryServeOnce
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => (TransportConfig BSTransport -> TransportConfig tp)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT u nid k rpkt tp m (Either SomeException ())
tryServeOnce mk sess = tryAny (serveOnce mk sess)

serveOnce
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => (TransportConfig BSTransport -> TransportConfig tp)
  -> SessionT u nid k rpkt tp m ()
  -> ServerT u nid k rpkt tp m ()
serveOnce mk sess = do
  ServerEnv {..} <- ask
  (bs, addr) <- liftIO $ recvFrom serveSock 1024

  bsHandle <- HM.lookup bsHandleList $ show addr
  case bsHandle of
    Just h  -> feed h bs
    Nothing -> void $ async $ do
      connEnv <- newConnEnv mk addr bs
      mnid <- liftIO $ prepare addr connEnv
      forM_ mnid $ \(nid, uEnv) ->
        void $ handleConn "Client" addr connEnv nid uEnv sess

newConnEnv
  :: (MonadIO m, Transport tp)
  => (TransportConfig BSTransport -> TransportConfig tp)
  -> SockAddr
  -> ByteString
  -> ServerT u nid k rpkt tp m (ConnEnv tp)
newConnEnv mk addr bs = do
  ServerEnv {..} <- ask
  h <- newBSHandle bs
  HM.insert bsHandleList (show addr) h
  initConnEnv (mk (bsTransportConfig h (flip (sendAllTo serveSock) addr)))


newClient
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => (TransportConfig BSTransport -> TransportConfig tp)
  -> String
  -> nid
  -> u
  -> SessionT u nid k rpkt tp m ()
  -> ServerT u nid k rpkt tp m (Maybe (NodeEnv1 u nid k rpkt tp))
newClient mk hostPort nid uEnv sess = do
  addr <- liftIO $ getDatagramAddr hostPort
  case addr of
    Nothing -> do
      liftIO $ errorM "Metro.UDP" $ "Connect UDP Server " ++ hostPort ++ " failed"
      return Nothing
    Just addr0 -> do
      connEnv <- newConnEnv mk (addrAddress addr0) empty
      Just <$> handleConn "Server" (addrAddress addr0) connEnv nid uEnv sess

handleConn
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => String
  -> SockAddr
  -> ConnEnv tp
  -> nid
  -> u
  -> SessionT u nid k rpkt tp m ()
  -> ServerT u nid k rpkt tp m (NodeEnv1 u nid k rpkt tp)
handleConn n addr connEnv nid uEnv sess = do
    ServerEnv {..} <- ask

    liftIO $ infoM "Metro.UDP" (serveName ++ n ++ ": " ++ show nid ++ " connected")
    env0 <- initEnv1 nodeMode connEnv uEnv nid defSessTout gen

    env1 <- atomically $ do
      v <- HM.lookupSTM nodeEnvList nid
      HM.insertSTM nodeEnvList nid env0
      pure v

    mapM_ (`runNodeT1` stopNodeT) env1

    void $ async $ do
      lift . runNodeT1 env0 $ startNodeT sess
      HM.delete bsHandleList (show addr)
      liftIO $ infoM "Metro.UDP" (serveName ++ n ++ ": " ++ show nid ++ " disconnected")

    return env0

startServer
  :: (MonadUnliftIO m, Transport tp, Show nid, Eq nid, Hashable nid, Eq k, Hashable k, GetPacketId k rpkt, RecvPacket rpkt)
  => ServerEnv u nid k rpkt tp
  -> (TransportConfig BSTransport -> TransportConfig tp)
  -> SessionT u nid k rpkt tp m ()
  -> m ()
startServer sEnv mk sess = do
  when (keepalive sEnv > 0) $ runCheckNodeState (keepalive sEnv) (nodeEnvList sEnv)
  runServerT sEnv $ serveForever mk sess
  liftIO $ Socket.close $ serveSock sEnv

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

dropS :: String -> String
dropS = drop 3 . dropWhile (/= ':')

toMaybe :: String -> Maybe String
toMaybe [] = Nothing
toMaybe xs = Just xs

getHost :: String -> Maybe String
getHost = toMaybe . takeWhile (/=':') . dropS

getService :: String -> Maybe String
getService = toMaybe . drop 1 . dropWhile (/=':') . dropS
