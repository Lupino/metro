{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-- | This module provides convenience functions for interfacing @tls@.
--
-- This module is intended to be imported @qualified@, e.g.:
--
module Metro.TP.TLS
  ( TLS
    -- * re-export
  , module Metro.TP.TLSSetting
  , tlsConfig
  ) where

import           Control.Exception     (SomeException, bracketOnError, catch)
import qualified Data.ByteString.Char8 as B (append, length, null)
import qualified Data.ByteString.Lazy  as BL (fromStrict)
import           Metro.Class           (Transport (..))
import           Metro.TP.TLSSetting
import           Network.TLS           (Context, TLSParams)
import qualified Network.TLS           as TLS


data TLS = TLS (IO String) Context

instance Transport TLS where
  data TransportConfig TLS = forall params tp. (Transport tp, TLSParams params) => TLSConfig params (TransportConfig tp)

  -- | Convenience function for initiating an TLS transport
  --
  -- This operation may throw 'TLS.TLSException' on failure.
  --
  newTP (TLSConfig params config) = do
    transport <- newTP config
    bracketOnError (TLS.contextNew (transportBackend transport) params) closeTLS $ \ctx -> do
      TLS.handshake ctx
      return $ TLS (getTPName transport) ctx

  recvData (TLS _ ctx) = const $ TLS.recvData ctx
  sendData (TLS _ ctx) = TLS.sendData ctx . BL.fromStrict
  closeTP (TLS _ ctx) = closeTLS ctx
  getTPName (TLS tp _)  = tp

transportBackend :: Transport tp => tp -> TLS.Backend
transportBackend transport = TLS.Backend
  { TLS.backendFlush = return ()
  , TLS.backendClose = closeTP transport
  , TLS.backendSend = sendData transport
  , TLS.backendRecv = recvData'
  }

  where recvData' nbytes = do
         s <- recvData transport nbytes
         if loadMore nbytes s then do
                              s' <- recvData' (nbytes - B.length s)
                              return $ s `B.append` s'
                              else return s

        loadMore nbytes bs | B.null bs = False
                           | B.length bs < nbytes = True
                           | otherwise = False


-- | Close a TLS 'Context' and its underlying socket.
--
closeTLS :: Context -> IO ()
closeTLS ctx = (TLS.bye ctx >> TLS.contextClose ctx) -- sometimes socket was closed before 'TLS.bye'
    `catch` (\(_::SomeException) -> return ())   -- so we catch the 'Broken pipe' error here


tlsConfig :: (Transport tp, TLSParams params) => params -> TransportConfig tp -> TransportConfig TLS
tlsConfig = TLSConfig
