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

testNormalCleanup :: IO ()
testNormalCleanup = withNode $ \node _ -> do
  N.runNodeT1 node $ N.withSessionT_ (pure 7) Nothing $ pure ()
  count <- N.getSessionSize1 node
  assert "A completed session must release its own entry" (count == 0)

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
     ("Normal session cleanup", testNormalCleanup),
     ("Closed connection writes", testClosedSend)]
  assert "Node lifecycle regressions failed" (and results)
