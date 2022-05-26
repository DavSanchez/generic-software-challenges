-- | This contains the functions related to channels and buffers, that is, the channel to which the Server writes
-- and from which the Consumer reads, and also the set of currently stored 'Int's that the Consumer compares with
-- the incoming 'Int's. As these constructs are a shared resource to be used by several threads, concurrency capabilities offered by [Software Transactional Memory](https://hackage.haskell.org/package/stm).
--
-- == Comments
-- One could wonder why an in-memory @IntSet@ was chosen to check for duplicates instead of other solutions. The answer is the time is limited, and an @IntSet@ would perform adequately at the beginning of the program, as operations within an @IntSet@ have a worst-case complexity of /O(min(n,W))/. This means that the operation can become linear in the number of elements with a maximum of /W/ -- the number of bits in an 'Int' (32 or 64). For more information on alternatives considered, you can check "Internal.Consumer".
module Internal.Buffers
  ( -- * Types
    Queue,
    InsertedStore,

    -- * 'Queue' functions
    createQueue,
    insertInQueue,
    closeQueue,

    -- * 'InsertedStore' functions
    createInsertedStore,
    isDuplicate,
    addElement,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, isClosedTMQueue, newTMQueueIO, peekTMQueue, writeTMQueue)
import qualified Data.IntSet as IS

-- | This is the type alias for an unbounded, closeable FIFO queue.
type Queue = TMQueue Int

-- | Container for an in-memory store of already input numbers.
type InsertedStore = TVar IS.IntSet

-- | Creates a 'Queue'.
createQueue :: IO Queue
createQueue = newTMQueueIO

-- | Pushes a value in the 'Queue'.
insertInQueue :: Queue -> Int -> IO ()
insertInQueue c n = atomically $ writeTMQueue c n

-- |  Closes the 'Queue'.
closeQueue :: Queue -> IO ()
closeQueue b = atomically (closeTMQueue b)

-- | Creates a transactional variable ('TVar') storing a set of 'Int's ('IS.IntSet', wrapped on an 'InsertedStore').
createInsertedStore :: IO InsertedStore
createInsertedStore = newTVarIO IS.empty

-- | Checks if a number already exists in the 'IS.IntSet', which means that it has been already read before.
isDuplicate :: IS.IntSet -> Int -> Bool
isDuplicate = flip IS.member

-- | Adds an element to the 'IS.IntSet', effectively identifying all future occurrences of this number as duplicates.
addElement :: Int -> IS.IntSet -> IS.IntSet
addElement = IS.insert

-- | Debug function. Checks if the queue is closed.
_isClosed :: Queue -> IO Bool
_isClosed = atomically . isClosedTMQueue

-- | Debug function. Reads the value from the queue and prints it, but __the value isn't extracted from the queue__.
_readAndPrint :: Queue -> IO ()
_readAndPrint b = atomically (peekTMQueue b) >>= print