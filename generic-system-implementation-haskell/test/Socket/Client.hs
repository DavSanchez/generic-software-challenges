{-# LANGUAGE OverloadedStrings #-}

module Socket.Client (runTCPClient,sendAll) where

import qualified Control.Exception as E
import Network.Socket
    ( HostName,
      ServiceName,
      defaultHints,
      getAddrInfo,
      openSocket,
      withSocketsDo,
      connect,
      close,
      AddrInfo(addrSocketType, addrAddress),
      Socket,
      SocketType(Stream) )
import Network.Socket.ByteString (sendAll)

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
