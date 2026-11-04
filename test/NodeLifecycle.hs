{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless, void)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Metro.Class
import Metro.Conn (initConnEnv, runConnT, send)
import qualified Metro.Node as N
import qualified Metro.Session as S
import Metro.TP.BS (BSTP, makePipe)
import UnliftIO hiding (assert)

data Packet = Packet Word8 Word8 deriving (Eq, Show)

instance GetPacketId Word8 Packet where
  getPacketId (Packet sid _) = sid

instance RecvPacket () Packet where
  recvPacket _ _ recv = do
    bytes <- recv 2
    pure $ Packet (B.index bytes 0) (B.index bytes 1)

instance SendPacket Packet where
  sendPacket (Packet sid kind) write = write (B.pack [sid, kind])

type Node = N.NodeEnv1 () Int Word8 Packet BSTP

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ error label

withNode :: (Node -> (Packet -> IO ()) -> IO a) -> IO a
withNode action = do
  (serverConfig, peerConfig) <- makePipe "server" "peer"
  server <- initConnEnv serverConfig
  peer <- initConnEnv peerConfig
  size <- N.newPoolSize 1
  node <- N.initEnv1_ id server () (1 :: Int) True (pure (99 :: Word8)) size
  action node (runConnT peer . send) `finally` N.runNodeT1 node N.stopNodeT

testCleanup :: IO ()
testCleanup = withNode $ \node write -> do
  entered <- newEmptyMVar
  finish <- newEmptyMVar
  barrier <- newEmptyMVar
  let handler = do
        packet <- S.receive
        case packet of
          Just (Packet 7 1) -> liftIO $ putMVar entered () >> takeMVar finish
          Just (Packet 8 _) -> liftIO $ putMVar barrier ()
          _ -> pure ()
  withAsync (N.runNodeT1 node $ N.startNodeT handler) $ \_ -> do
    write (Packet 7 1)
    takeMVar entered
    ready <- newEmptyMVar
    withAsync (N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ do
      liftIO $ putMVar ready ()
      S.receive) $ \ack -> do
      takeMVar ready
      putMVar finish ()
      write (Packet 8 0)
      takeMVar barrier
      write (Packet 7 2)
      result <- timeout 500000 $ wait ack
      assert "SingleAction cleanup deleted the replacement ACK waiter" $
        result == Just (Just (Packet 7 2))

testAckUnderLoad :: IO ()
testAckUnderLoad = withNode $ \node write -> do
  entered <- newEmptyMVar
  finish <- newEmptyMVar
  let handler = do
        void S.receive
        liftIO $ putMVar entered () >> takeMVar finish
  withAsync (N.runNodeT1 node $ N.startNodeT handler) $ \_ -> do
    write (Packet 1 1)
    takeMVar entered
    ready <- newEmptyMVar
    withAsync (N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ do
      liftIO $ putMVar ready ()
      S.receive) $ \ack -> do
      takeMVar ready
      write (Packet 7 2)
      result <- timeout 500000 $ wait ack
      putMVar finish ()
      assert "A busy request pool starved an existing ACK waiter" $
        result == Just (Just (Packet 7 2))

testOwnership :: IO ()
testOwnership = withNode $ \node _ -> do
  N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $
    void $ liftIO $ N.runNodeT1 node $ N.newSessionEnv Nothing 7
  count <- N.getSessionSize1 node
  assert "An old session removed a newer session with the same ID" (count == 1)

testExclusiveSessionIds :: IO ()
testExclusiveSessionIds = withNode $ \node write -> do
  firstReady <- newEmptyMVar
  secondStarted <- newEmptyMVar
  secondReady <- newEmptyMVar
  let handler = void S.receive
  withAsync (N.runNodeT1 node $ N.startNodeT handler) $ \_ -> do
    withAsync (N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ do
      liftIO $ putMVar firstReady ()
      S.receive) $ \first -> do
      takeMVar firstReady
      withAsync (do
        putMVar secondStarted ()
        N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ do
          liftIO $ putMVar secondReady ()
          S.receive) $ \second -> do
        takeMVar secondStarted
        premature <- timeout 50000 $ readMVar secondReady
        assert "A second ACK waiter replaced a live same-ID session" $
          premature == Nothing
        write (Packet 7 2)
        firstResult <- timeout 500000 $ wait first
        assert "The first ACK was stolen by the second assignment" $
          firstResult == Just (Just (Packet 7 2))
        ready <- timeout 500000 $ takeMVar secondReady
        assert "The next assignment did not acquire the released ID" $
          ready == Just ()
        write (Packet 7 3)
        secondResult <- timeout 500000 $ wait second
        assert "The second assignment lost its own ACK" $
          secondResult == Just (Just (Packet 7 3))

testExclusiveWaitDisconnect :: IO ()
testExclusiveWaitDisconnect = withNode $ \node _ -> do
  firstReady <- newEmptyMVar
  secondStarted <- newEmptyMVar
  withAsync (N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ do
    liftIO $ putMVar firstReady ()
    S.receive) $ \_ -> do
    takeMVar firstReady
    withAsync (do
      putMVar secondStarted ()
      tryAny $ N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ pure ()) $ \second -> do
      takeMVar secondStarted
      premature <- timeout 50000 $ wait second
      assert "Same-ID acquisition did not wait for the current owner" $
        case premature of Nothing -> True; _ -> False
      N.runNodeT1 node N.stopNodeT
      result <- timeout 500000 $ wait second
      assert "A blocked session acquisition did not terminate on disconnect" $
        case result of
          Just (Left err) -> fromException err == Just TransportClosed
          _ -> False

testNormalCleanup :: IO ()
testNormalCleanup = withNode $ \node _ -> do
  N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ pure ()
  count <- N.getSessionSize1 node
  assert "A completed session must release its own entry" (count == 0)

testNodeCloseDrainsThreads :: IO ()
testNodeCloseDrainsThreads = withNode $ \node write -> do
  handlerEntered <- newEmptyMVar
  handlerExited <- newEmptyMVar
  never <- newEmptyMVar
  ackReady <- newEmptyMVar
  let handler = do
        void S.receive
        liftIO $ (putMVar handlerEntered () >> takeMVar never)
          `finally` putMVar handlerExited ()
  withAsync (N.runNodeT1 node $ N.startNodeT handler) $ \mainLoop -> do
    write (Packet 1 1)
    takeMVar handlerEntered
    withAsync (N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ do
      liftIO $ putMVar ackReady ()
      S.receive) $ \ack -> do
      takeMVar ackReady
      N.runNodeT1 node N.stopNodeT
      handlerDone <- timeout 500000 $ takeMVar handlerExited
      assert "Node close left a request-handler thread running" $
        handlerDone == Just ()
      ackDone <- timeout 500000 $ wait ack
      assert "Node close left an ACK waiter blocked" $
        ackDone == Just Nothing
      mainDone <- timeout 500000 $ wait mainLoop
      assert "Node close left the receive loop running" $
        mainDone == Just ()
      count <- N.getSessionSize1 node
      assert "Node close retained a completed ACK session" (count == 0)

testClosedSend :: IO ()
testClosedSend = withNode $ \node _ -> do
  N.runNodeT1 node N.stopNodeT
  N.runNodeT1 node N.stopNodeT
  result <- tryAny $ runConnT (N.connEnv node) $ send (Packet 7 1)
  assert "A closed connection must reject writes before touching the transport" $
    case result of
      Left err -> fromException err == Just TransportClosed
      Right () -> False

main :: IO ()
main = do
  results <- mapM (\(name, test) -> do
    result <- tryAny $ test
    case result of
      Left err -> putStrLn ("FAIL " ++ name ++ ": " ++ show err) >> pure False
      Right () -> putStrLn ("PASS " ++ name) >> pure True)
    [("SingleAction cleanup", testCleanup),
     ("ACK under request load", testAckUnderLoad),
     ("Session ownership", testOwnership),
     ("Exclusive session IDs", testExclusiveSessionIds),
     ("Exclusive wait disconnect", testExclusiveWaitDisconnect),
     ("Normal session cleanup", testNormalCleanup),
     ("Node close drains threads", testNodeCloseDrainsThreads),
     ("Closed connection writes", testClosedSend)]
  assert "Node lifecycle regressions failed" (and results)
