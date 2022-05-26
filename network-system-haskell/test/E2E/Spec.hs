{-# LANGUAGE OverloadedStrings #-}

module E2E.Spec (specs) where

import qualified Buffers as B
import qualified Consumer as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, wait)
import qualified Control.Exception as E
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.List (sort)
import qualified Input.Props as InputP
import qualified Logging as L
import qualified Server as S
import qualified Socket.Client as Client
import System.IO (hPutStr, stderr)
import Test.Hspec (SpecWith, describe, it, shouldBe)

specs :: [SpecWith ()]
specs =
  [ spec_serverRunGood
  ]

spec_serverRunGood :: SpecWith ()
spec_serverRunGood = describe "Server accepts values and they get printed to a file" $ do
  it "Store simple input" $ do
    let numbers = [1 .. 10000]
        input = serverInputs numbers
    server <- async runApplication
    sendInputToServer input 100
    terminateServer
    wait server
    outputFileNumbers <- sort . map (read :: String -> Int) . lines <$> readFile destinationTestFile
    outputFileNumbers `shouldBe` numbers

_specServerRunBad :: SpecWith ()
_specServerRunBad = describe "Server only sends good content to file" $ do
  it "With bad input" $ do
    let goodNumbers = [1 .. 10]
        goodInput = serverInputs goodNumbers
        badInput = ["bad input" <> C.singleton '\n'] <> goodInput
    server <- async runApplication
    sendInputToServer goodInput 10
    sendInputToServer badInput 10
    terminateServer
    wait server
    outputFileNumbers <- sort . map (read :: String -> Int) . lines <$> readFile destinationTestFile
    outputFileNumbers `shouldBe` goodNumbers

destinationTestFile :: FilePath
destinationTestFile = "test/E2E/numbers.log"

serverInputs :: [Int] -> [BS.ByteString]
serverInputs = map InputP.toBSInput

terminateServer :: IO ()
terminateServer = Client.runTCPClient "localhost" serverPort $ \s -> do
  Client.sendAll s ("terminate" <> C.singleton '\n')
  threadDelay 1000000

sendInputToServer :: [BS.ByteString] -> Int -> IO ()
sendInputToServer inputs delay = Client.runTCPClient "localhost" serverPort $ \s -> do
  forM_ inputs $ \input -> do
    -- BS.appendFile "test/E2E/inputs.log" input -- For debugging
    Client.sendAll s input
    threadDelay delay

maxConnections :: Int
maxConnections = 5

serverPort :: S.Port
serverPort = "4000"

runApplication :: IO ()
runApplication = do
  E.catch (E.bracket getResources releaseResources (run serverPort maxConnections)) handleServerCancellation

getResources :: IO (B.Queue, Async ())
getResources = do
  L.cleanupFile destinationTestFile
  q <- B.createQueue
  consumer <- async (C.consumeAndProcess q destinationTestFile)
  pure (q, consumer)

releaseResources :: (B.Queue, Async ()) -> IO ()
releaseResources (b, c) = do
  B.closeQueue b
  wait c

run :: S.Port -> Int -> (B.Queue, Async ()) -> IO ()
run port maxConn (b, _) = S.serve port maxConn b

handleServerCancellation :: E.SomeException -> IO ()
handleServerCancellation _ = hPutStr stderr "Closing program"