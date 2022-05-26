{- This is for more extensive (multithreaded, etc) testing the real application -}

{-# LANGUAGE OverloadedStrings #-}

module E2E.StandAlone (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import qualified Control.Exception as E
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import Network.Socket
  ( AddrInfo (addrAddress, addrSocketType),
    HostName,
    ServiceName,
    Socket,
    SocketType (Stream),
    close,
    connect,
    defaultHints,
    getAddrInfo,
    openSocket,
    withSocketsDo,
  )
import Network.Socket.ByteString (sendAll)

main :: IO ()
main = do
  let input1 = serverInputs [1 .. 10000]
      input2 = serverInputs [100 .. 10000]
      input3 = serverInputs (reverse [10000 .. 100000])
      input4 = serverInputs [0 .. 999999999]
      input5 = serverInputs (reverse [0 .. 999999999])
  mapConcurrently_ sendInputToServer [input1, input2, input3, input4, input5]
  terminateServer
  putStrLn "Client done :)"

-- destinationTestFile :: FilePath
-- destinationTestFile = "test/E2E/numbers.log"

serverInputs :: [Int] -> [BS.ByteString]
serverInputs = map toBSInput

terminateServer :: IO ()
terminateServer = runTCPClient "localhost" serverPort $ \s -> do
  sendAll s ("terminate" <> C.singleton '\n')
  threadDelay 1000000

sendInputToServer :: [BS.ByteString] -> IO ()
sendInputToServer inputs = runTCPClient "localhost" serverPort $ \s -> do
  forM_ inputs $ \input -> do
    -- BS.appendFile "test/E2E/inputs.log" input -- For debugging
    sendAll s input
    threadDelay 100

-- maxConnections :: Int
-- maxConnections = 5

toBSInput :: Int -> BS.ByteString
toBSInput = flip BS.append "\n" . justifyRight 9 48 . C.pack . show
  where
    justifyRight :: Int -> Word8 -> BS.ByteString -> BS.ByteString
    justifyRight k c t
      | BS.length t >= k = t
      | otherwise = BS.replicate (k - BS.length t) c `BS.append` t

serverPort :: ServiceName
serverPort = "4000"

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