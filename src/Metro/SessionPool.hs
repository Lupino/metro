{-# LANGUAGE RecordWildCards #-}
module Metro.SessionPool
  ( SessionPool
  , newSessionPool
  , setMaxPoolSize
  , spawn
  , close
  ) where


import           Control.Monad (forever)
import           UnliftIO


data SessionState rpkt = SessionState
  { statePacket :: Maybe rpkt
  , stateIsBusy :: Bool
  }


type PoolerState rpkt = TVar (SessionState rpkt)

data SessionPooler rpkt = SessionPooler
  { poolerState :: PoolerState rpkt
  , poolerIO    :: Async ()
  }


type FreeStates rpkt = TVar [PoolerState rpkt]


data SessionPool rpkt = SessionPool
  { poolerList  :: TVar [SessionPooler rpkt]
  , freeStates  :: FreeStates rpkt
  , packetList  :: TVar [rpkt]
  , maxPoolSize :: TVar Int
  }


newSessionPool :: MonadIO m => m (SessionPool rpkt)
newSessionPool = do
  poolerList  <- newTVarIO []
  freeStates  <- newTVarIO []
  packetList  <- newTVarIO []
  maxPoolSize <- newTVarIO 10
  pure SessionPool {..}


setMaxPoolSize :: MonadIO m => SessionPool rpkt -> Int -> m ()
setMaxPoolSize SessionPool {..} = atomically . writeTVar maxPoolSize


getFreeState :: MonadIO m => FreeStates rpkt -> m (Maybe (TVar (SessionState rpkt)))
getFreeState states = do
  atomically $ do
    ss <- readTVar states
    case ss of
      [] -> pure Nothing
      (x:xs) -> do
        writeTVar states xs
        pure $ Just x


startPoolerIO :: MonadUnliftIO m => TVar [rpkt] -> FreeStates rpkt -> PoolerState rpkt -> (rpkt -> m ()) -> m (Async ())
startPoolerIO packets states state work = async $ forever $ do
  rpkt <- atomically
    $ readTVar state
    >>= maybe retrySTM pure . statePacket

  work rpkt
  atomically $ do
    pkts <- readTVar packets
    case pkts of
      [] -> do
        modifyTVar' state $ \s -> s
          { statePacket = Nothing
          , stateIsBusy = False
          }

        modifyTVar' states (state:)
      (x:xs) -> do
        modifyTVar' state $ \s -> s
          { statePacket = Just x
          }
        writeTVar packets xs


spawn :: MonadUnliftIO m => SessionPool rpkt -> (rpkt -> m ()) -> rpkt -> m ()
spawn SessionPool {..} work rpkt = do
  mState <- getFreeState freeStates
  case mState of
    Just state -> atomically $ modifyTVar' state $ \s -> s
      { statePacket = Just rpkt
      , stateIsBusy = True
      }
    Nothing -> do
      size <- length <$> readTVarIO poolerList
      maxSize <- readTVarIO maxPoolSize
      if size < maxSize then do
        state <- newTVarIO SessionState
          { statePacket = Just rpkt
          , stateIsBusy = True
          }
        io <- startPoolerIO packetList freeStates state work
        atomically $ modifyTVar' poolerList (SessionPooler state io:)
      else atomically $ modifyTVar' packetList (rpkt:)

close :: MonadIO m => SessionPool rpkt -> m ()
close SessionPool {..} = readTVarIO poolerList >>= mapM_ (cancel . poolerIO)
