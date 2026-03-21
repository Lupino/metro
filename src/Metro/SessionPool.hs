{-# LANGUAGE RecordWildCards #-}
module Metro.SessionPool
  ( SessionPool
  , PoolSize (..)
  , newPoolSize
  , setPoolSize
  , newSessionPool
  , runSessionPool
  , spawn
  , close
  ) where


import           Control.Monad (forM_, unless, when)
import           Metro.Utils   (foreverExit, lift)
import           UnliftIO


data SessionState rpkt = SessionState
  { statePacket :: Maybe rpkt
  , stateIsBusy :: Bool
  , stateAlive  :: Bool
  }


type PoolerState rpkt = TVar (SessionState rpkt)

data SessionPooler rpkt = SessionPooler
  { poolerState :: PoolerState rpkt
  , poolerIO    :: Async ()
  }


type FreeStates rpkt = TVar [PoolerState rpkt]
newtype PoolSize = PoolSize (TVar Int)

newPoolSize :: MonadIO m => Int -> m PoolSize
newPoolSize size = PoolSize <$> newTVarIO (max 1 size)

getPoolSize :: PoolSize -> STM Int
getPoolSize (PoolSize h) = readTVar h

setPoolSize :: MonadIO m => PoolSize -> Int -> m ()
setPoolSize (PoolSize h) = atomically . writeTVar h . max 1


data SessionPool rpkt = SessionPool
  { poolerList      :: TVar [SessionPooler rpkt]
  , creatingPoolers :: TVar Int
  , freeStates      :: FreeStates rpkt
  , packetList      :: TVar [rpkt]
  , maxPoolSize     :: PoolSize
  , queue           :: TQueue (PoolerState rpkt)
  , nodeStatus      :: TVar Bool
  , myIO            :: TVar (Maybe (Async ()))
  }


newSessionPool
  :: MonadIO m
  => PoolSize -> TVar Bool -> m (SessionPool rpkt)
newSessionPool maxPoolSize nodeStatus = do
  poolerList <- newTVarIO []
  creatingPoolers <- newTVarIO 0
  freeStates <- newTVarIO []
  packetList <- newTVarIO []
  queue      <- newTQueueIO
  myIO       <- newTVarIO Nothing
  pure SessionPool {..}


getFreeState :: FreeStates rpkt -> STM (Maybe (TVar (SessionState rpkt)))
getFreeState states = do
  ss <- readTVar states
  case ss of
    [] -> pure Nothing
    (x:xs) -> do
      writeTVar states xs
      pure $ Just x


startPoolerIO :: MonadUnliftIO m => TVar [rpkt] -> FreeStates rpkt -> PoolerState rpkt -> (rpkt -> m ()) -> m (Async ())
startPoolerIO packets states state work =
  async $ foreverExit $ \exit -> do
    mrpkt <- atomically $ do
      st <- readTVar state
      case statePacket st of
        Just rpkt -> pure (Just rpkt)
        Nothing ->
          if stateAlive st
            then retrySTM
            else pure Nothing

    case mrpkt of
      Nothing -> exit ()
      Just rpkt -> do
        lift $ work rpkt

        atomically $ do
          pkts <- readTVar packets
          st <- stateAlive <$> readTVar state
          if st
            then case pkts of
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
            else
              modifyTVar' state $ \s -> s
                { statePacket = Nothing
                , stateIsBusy = False
                }

        st <- stateAlive <$> readTVarIO state
        unless st $ exit ()


spawn :: MonadIO m => SessionPool rpkt -> rpkt -> m ()
spawn SessionPool {..} rpkt = atomically $ do
    mState <- getFreeState freeStates
    case mState of
      Just state -> modifyTVar' state $ \s -> s
        { statePacket = Just rpkt
        , stateIsBusy = True
        }

      Nothing -> do
        running <- length <$> readTVar poolerList
        creating <- readTVar creatingPoolers
        let size = running + creating
        maxSize <- getPoolSize maxPoolSize
        if size < maxSize then do
          state <- newTVar SessionState
            { statePacket = Just rpkt
            , stateIsBusy = True
            , stateAlive  = True
            }
          modifyTVar' creatingPoolers (+1)
          writeTQueue queue state

        else do
          pl <- length <$> readTVar packetList
          if pl >= maxSize then retrySTM
                           else do
            modifyTVar' packetList (++ [rpkt])
            when (size > maxSize) $ do
              poolers <- readTVar poolerList
              writeTVar poolerList $! drop (size - maxSize) poolers
              forM_ (take (size - maxSize) poolers) $ \p ->
                modifyTVar' (poolerState p) $ \st -> st {stateAlive = False}


runSessionPool
  :: MonadUnliftIO m
  => SessionPool rpkt -> (rpkt -> m ()) -> m ()
runSessionPool SessionPool {..} work = do
  io <- async $ foreverExit $ \exit -> do
    mState <- atomically $ do
      st <- readTVar nodeStatus
      if st then Just <$> readTQueue queue
            else pure Nothing

    case mState of
      Nothing -> exit ()
      Just state -> do
        atomically $ modifyTVar' creatingPoolers (max 0 . subtract 1)
        io <- lift $ startPoolerIO packetList freeStates state work
        atomically $ modifyTVar' poolerList (SessionPooler state io:)

  atomically . writeTVar myIO $ Just io


close :: MonadIO m => SessionPool rpkt -> m ()
close SessionPool {..} = do
  readTVarIO poolerList >>= mapM_ (cancel . poolerIO)
  readTVarIO myIO >>= mapM_ cancel
