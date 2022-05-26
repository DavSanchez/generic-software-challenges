-- | Main module.
-- Contains the main function and functions regarding resource acquisition and release.
--
-- === Comment
-- Several points of improvement here. Aside from the obvious way of configuring the "Server" each time it's run by (for example) reading arguments from @stdin@ or some configuration file, the 'async' calls are called directly instead of the more 'E.bracket'-like, 'withAsync' function, which is the recommended way to spawn threads (in the same vein as 'E.bracket', this enforces what to do once the thread finishes, which we could miss when using the low-level 'async').
--
-- Also, the socket server stops on all thrown exceptions, some finer control over what 'E.Exception's are possible, with at least an explicit separation for the ones that we throw to stop the server intentionally, would be a good addition.
module Main (main) where

import qualified Buffers as B
import qualified Consumer as C
import Control.Concurrent.Async (Async, async, wait)
import qualified Control.Exception as E
import qualified Logging as L
import qualified Server as S
import System.IO (hPutStr, stderr)

-- | The maximum number of connections that the server will handle.
maxConnections :: Int
maxConnections = 5

-- | Port number.
serverPort :: S.Port
serverPort = "4000"

-- | The output file path.
outputFilePath :: FilePath
outputFilePath = "numbers.log"

-- | Main function.
-- Currently, this doesn't read any input from the user (for example, regarding the max number of connections, the destination file or the port to be used).
main :: IO ()
main = do
  putStrLn "Hello, world!"
  E.catch (E.bracket getResources releaseResources (run serverPort maxConnections)) handleServerCancellation

-- | Get the resources needed for the server.
-- This cleans up the destination file, leaving it empty. Also creates the closeable 'Queue' which the server will use to stream numbers for processing.
-- Lastly, it spawns a thread for consuming the queue.
getResources :: IO (B.Queue, Async ())
getResources = do
  L.cleanupFile outputFilePath
  q <- B.createQueue
  consumer <- async (C.consumeAndProcess q outputFilePath)
  pure (q, consumer)

-- | Release the resources used by the server.
-- On an exception (that can be caused by a legit 'I.Terminate' command by a connected client), this will close the queue
-- and wait for the consumer to finish processing the remaining messages.
releaseResources ::
  -- | 'Queue' and 'Consumer' thread
  (B.Queue, Async ()) ->
  IO ()
releaseResources (b, c) = do
  B.closeQueue b
  wait c

-- | Run the server with the maximum number of connections, the port name and the queue.
run ::
  -- | Port to listen to.
  S.Port ->
  -- | Maximum number of connections
  Int ->
  -- | 'Queue' and 'Consumer' thread
  (B.Queue, Async ()) ->
  IO ()
run port maxConn (b, _) = S.serve port maxConn b

-- | On an exception, after cleaning up the resources, this will print to stderr that the program is exiting.
handleServerCancellation ::
  -- | The server currently closes on any exception.
  E.SomeException ->
  IO ()
handleServerCancellation _ = hPutStr stderr "Closing program"
