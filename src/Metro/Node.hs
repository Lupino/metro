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
  , uEnv
  , NodeMode (..)
  , SessionMode (..)
  , NodeT
  , initEnv
  , withEnv

  , setNodeMode
  , setSessionMode
  , setDefaultSessionTimeout
  , setDefaultSessionTimeout1

  , runNodeT
  , startNodeT
  , startNodeT_
  , withSessionT
  , nodeState
  , stopNodeT
  , env
  , request
  , requestAndRetry

  , newSessionEnv
  , nextSessionId
  , runSessionT_

  , busy

  -- combine node env and conn env
  , NodeEnv1 (..)
  , initEnv1
  , runNodeT1
  , getEnv1

  , getTimer
  , getNodeId

  , getSessionSize
  , getSessionSize1
  ) where

import           Control.Monad              (forM, forever, void, when)
import           Control.Monad.Cont         (callCC, runContT)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.IOMap                 (IOMap)
import qualified Data.IOMap                 as Map (delete, elems, empty,
                                                    insert, lookup, size)
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe, isJust)
import           Metro.Class                (GetPacketId, RecvPacket,
                                             SendPacket, SetPacketId, Transport,
                                             getPacketId)
import           Metro.Conn                 (ConnEnv, ConnT, FromConn (..),
                                             close, receive, runConnT)
import           Metro.Session              (SessionEnv (sessionId), SessionT,
                                             feed, isTimeout, runSessionT)
import qualified Metro.Session              as S (newSessionEnv, receive, send)
import           Metro.Utils                (getEpochTime)
import           System.Log.Logger          (errorM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

data NodeMode = Single
    | Multi
    deriving (Show, Eq)

data SessionMode = SingleAction
    | MultiAction
    deriving (Show, Eq)


data NodeEnv u nid k rpkt = NodeEnv
    { uEnv        :: u
    , nodeStatus  :: TVar Bool
    , nodeMode    :: NodeMode
    , sessionMode :: SessionMode
    , nodeSession :: TVar (Maybe (SessionEnv u nid k rpkt))
    , sessionList :: IOMap k (SessionEnv u nid k rpkt)
    , sessionGen  :: IO k
    , nodeTimer   :: TVar Int64
    , nodeId      :: nid
    , onExcColse  :: Bool
    , sessTimeout :: TVar Int64
    , onNodeLeave :: TVar (Maybe (u -> IO ()))
    }

data NodeEnv1 u nid k rpkt tp = NodeEnv1
    { nodeEnv :: NodeEnv u nid k rpkt
    , connEnv :: ConnEnv tp
    }

newtype NodeT u nid k rpkt tp m a = NodeT { unNodeT :: ReaderT (NodeEnv u nid k rpkt) (ConnT tp m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (NodeEnv u nid k rpkt)
    )

instance MonadUnliftIO m => MonadUnliftIO (NodeT u nid k rpkt tp m) where
  withRunInIO inner = NodeT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runNodeT r)

instance MonadTrans (NodeT u nid k rpkt tp) where
  lift = NodeT . lift . lift

instance FromConn (NodeT u nid k rpkt) where
  fromConn = NodeT . lift

runNodeT :: NodeEnv u nid k rpkt -> NodeT u nid k rpkt tp m a -> ConnT tp m a
runNodeT nEnv = flip runReaderT nEnv . unNodeT

runNodeT1 :: NodeEnv1 u nid k rpkt tp -> NodeT u nid k rpkt tp m a -> m a
runNodeT1 NodeEnv1 {..} = runConnT connEnv . runNodeT nodeEnv

initEnv :: MonadIO m => u -> nid -> Bool -> IO k -> m (NodeEnv u nid k rpkt)
initEnv uEnv nodeId onExcColse sessionGen = do
  nodeStatus  <- newTVarIO True
  nodeSession <- newTVarIO Nothing
  sessionList <- Map.empty
  nodeTimer   <- newTVarIO =<< getEpochTime
  onNodeLeave <- newTVarIO Nothing
  sessTimeout <- newTVarIO 300
  pure NodeEnv
    { nodeMode    = Multi
    , sessionMode = SingleAction
    , ..
    }

withEnv :: (Monad m) =>  u -> NodeT u nid k rpkt tp m a -> NodeT u nid k rpkt tp m a
withEnv u m = do
  env0 <- ask
  fromConn $ runNodeT (env0 {uEnv=u}) m

setNodeMode :: NodeMode -> NodeEnv u nid k rpkt -> NodeEnv u nid k rpkt
setNodeMode mode nodeEnv = nodeEnv {nodeMode = mode}

setSessionMode :: SessionMode -> NodeEnv u nid k rpkt -> NodeEnv u nid k rpkt
setSessionMode mode nodeEnv = nodeEnv {sessionMode = mode}

setDefaultSessionTimeout :: TVar Int64 -> NodeEnv u nid k rpkt -> NodeEnv u nid k rpkt
setDefaultSessionTimeout t nodeEnv = nodeEnv { sessTimeout = t }

setDefaultSessionTimeout1 :: MonadIO m => NodeEnv1 u nid k rpkt tp -> Int64 -> m ()
setDefaultSessionTimeout1 NodeEnv1 {..} = atomically . writeTVar (sessTimeout nodeEnv)


initEnv1
  :: MonadIO m
  => (NodeEnv u nid k rpkt -> NodeEnv u nid k rpkt)
  -> ConnEnv tp -> u -> nid -> Bool -> IO k -> m (NodeEnv1 u nid k rpkt tp)
initEnv1 mapEnv connEnv uEnv nid excColse gen = do
  nodeEnv <- mapEnv <$> initEnv uEnv nid excColse gen
  return NodeEnv1 {..}

getEnv1
  :: (Monad m, Transport tp)
  => NodeT u nid k rpkt tp m (NodeEnv1 u nid k rpkt tp)
getEnv1 = do
  connEnv <- fromConn ask
  nodeEnv <- ask
  return NodeEnv1 {..}

runSessionT_ :: Monad m => SessionEnv u nid k rpkt -> SessionT u nid k rpkt tp m a -> NodeT u nid k rpkt tp m a
runSessionT_ aEnv = fromConn . runSessionT aEnv

withSessionT
  :: (MonadUnliftIO m, Eq k, Ord k)
  => Maybe Int64 -> SessionT u nid k rpkt tp m a -> NodeT u nid k rpkt tp m a
withSessionT sTout sessionT =
  bracket nextSessionId removeSession $ \sid -> do
    aEnv <- newSessionEnv sTout sid
    runSessionT_ aEnv sessionT

newSessionEnv :: (MonadIO m, Eq k, Ord k) => Maybe Int64 -> k -> NodeT u nid k rpkt tp m (SessionEnv u nid k rpkt)
newSessionEnv sTout sid = do
  NodeEnv{..} <- ask
  dTout <- readTVarIO sessTimeout
  sEnv <- S.newSessionEnv uEnv nodeId sid (fromMaybe dTout sTout) []
  case nodeMode of
    Single -> atomically $ do
      sess <- readTVar nodeSession
      case sess of
        Nothing -> writeTVar nodeSession $ Just sEnv
        Just _  -> do
          state <- readTVar nodeStatus
          when state retrySTM
    Multi -> Map.insert sid sEnv sessionList
  return sEnv

nextSessionId :: MonadIO m => NodeT u nid k rpkt tp m k
nextSessionId = liftIO =<< asks sessionGen

removeSession :: (MonadIO m, Eq k, Ord k) => k -> NodeT u nid k rpkt tp m ()
removeSession mid = do
  NodeEnv{..} <- ask
  case nodeMode of
    Single -> atomically $ writeTVar nodeSession Nothing
    Multi  -> Map.delete mid sessionList

busy :: MonadIO m => NodeT u nid k rpkt tp m Bool
busy = do
  NodeEnv{..} <- ask
  case nodeMode of
    Single -> isJust <$> readTVarIO nodeSession
    Multi  -> return False

tryMainLoop
  :: (MonadUnliftIO m, Transport tp, RecvPacket rpkt, GetPacketId k rpkt, Eq k, Ord k)
  => (rpkt -> m Bool) -> SessionT u nid k rpkt tp m () -> NodeT u nid k rpkt tp m ()
tryMainLoop preprocess sessionHandler = do
  r <- tryAny $ mainLoop preprocess sessionHandler
  case r of
    Left err -> do
      case show err of
        "TransportClosed" -> stopNodeT
        ('N':'e':'t':'w':'o':'r':'k':_) -> stopNodeT
        errS -> do
          liftIO $ errorM "Metro.Node" $ "MainLoop Error: " ++ errS
          close <- asks onExcColse
          when close stopNodeT

    Right _  -> pure ()

mainLoop
  :: (MonadUnliftIO m, Transport tp, RecvPacket rpkt, GetPacketId k rpkt, Eq k, Ord k)
  => (rpkt -> m Bool) -> SessionT u nid k rpkt tp m () -> NodeT u nid k rpkt tp m ()
mainLoop preprocess sessionHandler = do
  NodeEnv{..} <- ask
  rpkt <- fromConn receive
  setTimer =<< getEpochTime
  r <- lift $ preprocess rpkt
  when r $ void . async $ tryDoFeed rpkt sessionHandler

tryDoFeed
  :: (MonadUnliftIO m, Transport tp, GetPacketId k rpkt, Eq k, Ord k)
  => rpkt -> SessionT u nid k rpkt tp m () -> NodeT u nid k rpkt tp m ()
tryDoFeed rpkt sessionHandler = do
  r <- tryAny $ doFeed rpkt sessionHandler
  case r of
    Left e  ->
      case show e of
        ('N':'e':'t':'w':'o':'r':'k':_) -> stopNodeT
        ee -> liftIO $ errorM "Metro.Node" $ "DoFeed Error: " ++ ee
    Right _ -> pure ()

doFeed
  :: (MonadUnliftIO m, GetPacketId k rpkt, Eq k, Ord k)
  => rpkt -> SessionT u nid k rpkt tp m () -> NodeT u nid k rpkt tp m ()
doFeed rpkt sessionHandler = do
  NodeEnv{..} <- ask
  v <- case nodeMode of
         Single -> readTVarIO nodeSession
         Multi  -> Map.lookup (getPacketId rpkt) sessionList
  case v of
    Just aEnv ->
      runSessionT_ aEnv $ feed $ Just rpkt
    Nothing    -> do
      let sid = getPacketId rpkt
      dTout <- readTVarIO sessTimeout
      sEnv <- S.newSessionEnv uEnv nodeId sid dTout [Just rpkt]
      when (sessionMode == MultiAction) $
        case nodeMode of
          Single -> atomically $ writeTVar nodeSession $ Just sEnv
          Multi  -> Map.insert sid sEnv sessionList
      bracket (return sid) removeSession $ \_ ->
        runSessionT_ sEnv sessionHandler

startNodeT
  :: (MonadUnliftIO m, Transport tp, RecvPacket rpkt, GetPacketId k rpkt, Eq k, Ord k)
  => SessionT u nid k rpkt tp m () -> NodeT u nid k rpkt tp m ()
startNodeT = startNodeT_ (const $ return True)

startNodeT_
  :: (MonadUnliftIO m, Transport tp, RecvPacket rpkt, GetPacketId k rpkt, Eq k, Ord k)
  => (rpkt -> m Bool) -> SessionT u nid k rpkt tp m () -> NodeT u nid k rpkt tp m ()
startNodeT_ preprocess sessionHandler = do
  sess <- runCheckSessionState
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    alive <- lift nodeState
    if alive then lift $ tryMainLoop preprocess sessionHandler
             else exit ()

  cancel sess
  doFeedError

nodeState :: MonadIO m => NodeT u nid k rpkt tp m Bool
nodeState = readTVarIO =<< asks nodeStatus

doFeedError :: MonadIO m => NodeT u nid k rpkt tp m ()
doFeedError =
  asks sessionList >>= Map.elems >>= mapM_ go
  where go :: MonadIO m => SessionEnv u nid k rpkt -> NodeT u nid k rpkt tp m ()
        go aEnv = runSessionT_ aEnv $ feed Nothing

stopNodeT :: (MonadIO m, Transport tp) => NodeT u nid k rpkt tp m ()
stopNodeT = do
  st <- asks nodeStatus
  atomically $ writeTVar st False
  fromConn close

env :: Monad m => NodeT u nid k rpkt tp m u
env = asks uEnv

request
  :: (MonadUnliftIO m, Transport tp, SendPacket spkt, SetPacketId k spkt, Eq k, Ord k)
  => Maybe Int64 -> spkt -> NodeT u nid k rpkt tp m (Maybe rpkt)
request sTout = requestAndRetry sTout Nothing

requestAndRetry
  :: (MonadUnliftIO m, Transport tp, SendPacket spkt, SetPacketId k spkt, Eq k, Ord k)
  => Maybe Int64 -> Maybe Int -> spkt -> NodeT u nid k rpkt tp m (Maybe rpkt)
requestAndRetry sTout retryTout spkt = do
  alive <- nodeState
  if alive then
    withSessionT sTout $ do
      S.send spkt
      t <- forM retryTout $ \tout ->
        async $ forever $ do
          threadDelay $ tout * 1000 * 1000
          S.send spkt
      ret <- S.receive
      mapM_ cancel t
      return ret


  else return Nothing

getTimer :: MonadIO m => NodeT u nid k rpkt tp m Int64
getTimer = readTVarIO =<< asks nodeTimer

setTimer :: MonadIO m => Int64 -> NodeT u nid k rpkt tp m ()
setTimer t = do
  v <- asks nodeTimer
  atomically $ writeTVar v t

getNodeId :: Monad m => NodeT n nid k rpkt tp m nid
getNodeId = asks nodeId

runCheckSessionState :: (MonadUnliftIO m, Eq k, Ord k) => NodeT u nid k rpkt tp m (Async ())
runCheckSessionState = do
  sessList <- asks sessionList
  async . forever $ do
    threadDelay $ 1000 * 1000 * 10  -- 10 seconds
    mapM_ (checkAlive sessList) =<< Map.elems sessList

  where checkAlive
          :: (MonadUnliftIO m, Eq k, Ord k)
          => IOMap k (SessionEnv u nid k rpkt) -> SessionEnv u nid k rpkt -> NodeT u nid k rpkt tp m ()
        checkAlive sessList sessEnv =
          runSessionT_ sessEnv $ do
            to <- isTimeout
            when to $ do
              feed Nothing
              Map.delete (sessionId sessEnv) sessList

getSessionSize :: MonadIO m => NodeEnv u nid k rpkt -> m Int
getSessionSize NodeEnv {..} = Map.size sessionList

getSessionSize1 :: MonadIO m => NodeEnv1 u nid k rpkt tp -> m Int
getSessionSize1 NodeEnv1 {..} = getSessionSize nodeEnv
