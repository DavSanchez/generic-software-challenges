{-# LANGUAGE DerivingStrategies #-}

-- | Module that exports the socket server functions to run and operate this component, the main component of the application together with "Consumer".
--
-- === Comments
-- The lengthiest module, "Server" can be considered the main component of the application, even more central than "Consumer". This uses the [network](https://hackage.haskell.org/package/network) library, with the low-level server implementation similar to what is suggested in its documentation, along with the [async](https://hackage.haskell.org/package/async) and [stm](https://hackage.haskell.org/package/stm) libraries for concurrency.
module Internal.Server
  ( -- * Types
    Port,
    ServerNextState,

    -- * Run server function
    serve,

    -- * Components
    process,
    receive,
    server,

    -- * Input processing
    processInput,

    -- * Low level functions
    runTCPServer,
    resolve,
    openServerSocket,
    shouldKeepRunning,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race, race_)
import Control.Concurrent.STM
  ( STM,
    TChan,
    TVar,
    atomically,
    check,
    modifyTVar,
    newTChan,
    newTVarIO,
    readTChan,
    readTVar,
    readTVarIO,
    writeTChan,
    writeTVar,
  )
import qualified Control.Exception as E
import Control.Monad (forever, join, void)
import qualified Data.ByteString as BS
import qualified Data.String as S
import qualified Internal.Buffers as B (Queue, insertInQueue)
import qualified Internal.Input as I
import Network.Socket
  ( AddrInfo (addrAddress, addrFlags, addrSocketType),
    AddrInfoFlag (AI_PASSIVE),
    ServiceName,
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultHints,
    getAddrInfo,
    gracefulClose,
    listen,
    openSocket,
    setCloseOnExecIfNeeded,
    setSocketOption,
    withFdSocket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv, sendAllTo)

-- | Abstract type that defines a signal for the server operation. It can be signalled to continue running or to terminate
data ServerNextState
  = -- |
    KeepRunning
  | -- |
    Terminate
  deriving stock (Eq)

-- | Port type alias (for ease of code reading).
type Port = ServiceName

-- | Run the server
serve ::
  -- | Port to listen to.
  Port ->
  -- | Maximum number of client connections.
  Int ->
  -- | Queue to which the server inserts the received values.
  B.Queue ->
  IO ()
serve port maxConn q = runTCPServer port maxConn (process q)

-- | Server start and activity function, spawns 2 threads for each client connection using 'race_'. These two components communicate via a 'TChan'.
process ::
  -- | Queue to which the server inserts the received values.
  B.Queue ->
  -- | Signals if the server should keep running or terminate.
  TVar ServerNextState ->
  -- | Socket object created by 'openServerSocket'.
  Socket ->
  IO ()
process q nextState s = do
  ch <- atomically newTChan
  race_ (server q nextState ch) (receive s ch)
  pure ()

-- | Receives value entered by the client.
receive ::
  -- | Socket object created by 'openServerSocket'.
  Socket ->
  -- | Channel for this client thread to communicate with its server thread.
  TChan BS.ByteString ->
  IO ()
receive sock ch = forever $ do
  msg <- recv sock 1024
  atomically $ writeTChan ch msg

-- | Checks if the server should be running via 'ServerNextState', if signalled to terminate, closes the thread, else reads the 'TChan' for input from the client and parses it. If the parse is correct the value is accepted as a valid input and processed, after which the thread loops again.
server ::
  -- | Queue to which the server inserts the received values.
  B.Queue ->
  -- | Signals if the server should keep running or terminate.
  TVar ServerNextState ->
  -- | Channel for this server thread to communicate with its client thread.
  TChan BS.ByteString ->
  IO ()
server q nextState chan = loop
  where
    loop = do
      join $ -- `join` is the same as `do bs <- bss` and then `bs`.
        atomically $ do
          cmd' <- readTVar nextState
          case cmd' of
            Terminate -> pure empty -- do nothing and break from the loop.
            KeepRunning -> do
              l <- readTChan chan
              pure (processI l)
    processI l = do
      case I.parseMaybeInput l of
        Nothing -> pure () -- Disconnect silently
        Just val -> processInput q val nextState >> loop

-- | Process the (currently two) different kinds of valid inputs. If 'I.Terminate', this activates the termination signal to be seen from all spawned server threads. If a valid number, then it is inserted in the 'B.Queue'.
processInput ::
  -- | Queue to which the server inserts the received values.
  B.Queue ->
  -- | Parsed input received from clients.
  I.Input ->
  -- | Signals if the server should keep running or terminate. Only this function will modify this value.
  TVar ServerNextState ->
  IO ()
processInput _ I.Terminate serverN = atomically (writeTVar serverN Terminate)
processInput q (I.Number n) _ = B.insertInQueue q n

-- | Function to run the actual socket server. This leverages the [network](https://hackage.haskell.org/package/network) library. Tracks the number of active connections. Every connection exceeding this value is sent a message informing it that the server is full and then disconnected. The server gracefully closes and re-throws any 'E.Exception', including the one which is raised when the server is instructed to @Terminate@, which returns control to the @main@ function.
runTCPServer ::
  -- | Port to listen to.
  Port ->
  -- | Maximum number of concurrent connections.
  Int ->
  -- | Callback function to operating with the 'Socket' object.
  (TVar ServerNextState -> Socket -> IO a) ->
  IO a
runTCPServer port maxConn serverProcess = withSocketsDo $ do
  numConnections <- newTVarIO (0 :: Int)
  addr <- resolve Stream port True
  serverStatus <- newTVarIO KeepRunning -- Every thread can modify this to stop the server
  E.bracket (open addr) close (loop serverStatus numConnections)
  where
    open addr = E.bracketOnError (openServerSocket addr) close $ \sock -> do
      listen sock 1024
      pure sock
    loop status lim sock = forever $ do
      result <- race (atomically $ shouldKeepRunning status) $
        E.bracketOnError (accept sock) (closeWithLimit lim . fst) $ do
          \(conn, _peer) -> do
            activeConnections <- readTVarIO lim
            if activeConnections < maxConn
              then do
                atomically (modifyTVar lim succ)
                void $ forkFinally (serverProcess status conn) (const $ gracefulCloseWithLimit lim conn)
              else do
                sendAllTo conn (S.fromString "Server full at the moment. Disconnecting...\n") _peer
                gracefulClose conn 5000
      either (const empty) pure result -- This throws an user exception if the signal to terminate is activated.
    closeWithLimit lim sock = atomically (modifyTVar lim pred) >> close sock
    gracefulCloseWithLimit lim sock = atomically (modifyTVar lim pred) >> gracefulClose sock 5000

-- |  Define the socket type and host information.
resolve :: SocketType -> Port -> Bool -> IO AddrInfo
resolve socketType port passive =
  head <$> getAddrInfo (Just hints) Nothing (Just port)
  where
    hints =
      defaultHints
        { addrSocketType = socketType,
          addrFlags = [AI_PASSIVE | passive] -- equivalent list comprehension of `if passive then [AI_PASSIVE] else []`
        }

-- | Creates a 'Socket' object.
openServerSocket :: AddrInfo -> IO Socket
openServerSocket addr = E.bracketOnError (openSocket addr) close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  return sock

-- | Called concurrently from 'runTCPServer', monitors the signal for a change. On @ServerNextState = Terminate@ it shuts down the server.
shouldKeepRunning :: TVar ServerNextState -> STM ()
shouldKeepRunning s = do
  s' <- readTVar s
  check (s' == Terminate)
