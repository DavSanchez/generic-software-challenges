{-# LANGUAGE OverloadedStrings #-}

-- | This is the interface module for the functions related to the Consumer, that is,
-- the element that reads from the 'B.Queue' exported by "Buffers" and processes its elements in a streaming pipeline.
-- The current solution uses streaming abstractions offered by the [conduit](https://hackage.haskell.org/package/conduit) library.
--
-- == Comments
-- I think a streaming-based processing of inputs coming from the channel is a good solution. The check to know if an 'Int' is a
-- duplicate is currently based on an @IntSet@, which is good on the short-term run of the application, but could cause a bottleneck once the numbers inserted approach the complete set @[0,999999999]@. Another approach could be storing all the inputs without filter in an auxiliary file using another @Conduit@. When inserting on the actual destination file, a constant-memory, @conduit@-based search of the auxiliary file for the duplicate number would perform better in the long run. However! I'm not that proficient with the @conduit@ library (yet!) to make this work without losing too much of the assignment time.
module Internal.Consumer
  ( -- * Consumer wrapper
    consumeAndProcess,

    -- * Processor alternatives
    _consumeAndProcess',
    _consumeAndProcess'',

    -- * Filtering
    isNotPresent,

    -- * Debug
    _sourceBufferDebug,
  )
where

import Conduit (ConduitT, ResourceT, filterMC, mapC, runConduitRes, sinkFileBS, unlinesAsciiC, yield, (.|))
import Control.Applicative (Alternative (empty))
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Control.Concurrent.STM.TMQueue (readTMQueue)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C
import Data.Conduit.TQueue (sourceTMQueue)
import qualified Internal.Buffers as B
import qualified Internal.Logging as L
import qualified Internal.Report as R

-- | This is a wrapper function for the actual function used in this module, which is currently '_consumeAndProcess'''
consumeAndProcess :: B.Queue -> FilePath -> IO ()
consumeAndProcess = _consumeAndProcess''

-- | /Manual/ streaming option, reads from the Queue, checks for duplicated value and appends to the destination file.
-- Only this is the only function needed from this module when using this option (current option is '_consumeAndProcess''') instead.
_consumeAndProcess' ::
  -- | Queue defined in "Internal.Buffers"
  B.Queue ->
  -- | Destination file name
  FilePath ->
  IO ()
_consumeAndProcess' q file = do
  alreadyInserted <- B.createInsertedStore
  report <- R.createReport
  _ <- async $ R.printReport report
  loop q report alreadyInserted
  where
    loop b r a = join $
      atomically $ do
        value <- readTMQueue b
        case value of
          Nothing -> pure empty
          Just v -> do
            inserted <- readTVar a
            if B.isDuplicate inserted v
              then pure $ do
                report' r True
                loop b r a
              else pure $ do
                report' r False
                L.logValue file v
                atomically $ modifyTVar a (B.addElement v)
                loop b r a
    report' r True = atomically (R.reportOnDuplicate r)
    report' r False = atomically (R.reportOnNew r)

-- | Streaming function that uses the [conduit](https://hackage.haskell.org/package/conduit) library to define a processing pipeline.
-- While the queue is not closed, it reads from it, performs a monadic filtering of the value (defined by the 'isNotPresent' function), inserts a newline character and then drops it in the destination file
_consumeAndProcess'' ::
  -- | Queue defined in "Internal.Buffers"
  B.Queue ->
  -- | Destination file name
  FilePath ->
  IO ()
_consumeAndProcess'' b file = do
  alreadyInserted <- B.createInsertedStore
  report <- R.createReport
  _ <- async $ R.printReport report
  runConduitRes $
    sourceTMQueue b
      .| filterMC (isNotPresent report alreadyInserted)
      .| mapC (C.pack . show)
      .| unlinesAsciiC
      .| sinkFileBS file

-- | Pipeline filtering component. Checks if a value has been inserted to this pipeline before. If so, reports a duplicate occurrence and produces a 'False' value, which discards the value from the pipeline. Else, it reports a new occurrence (inserting the number in the set in the process) and returns a 'True' value, thus keeping the value in the pipeline for storing in the destination file.
isNotPresent :: R.Report -> B.InsertedStore -> Int -> ResourceT IO Bool
isNotPresent rep is n = liftIO $
  atomically $ do
    inserted <- readTVar is
    let new = not $ B.isDuplicate inserted n
    if new
      then R.reportOnNew rep >> modifyTVar is (B.addElement n)
      else R.reportOnDuplicate rep
    pure new

-- | This function has the same behaviour as 'sourceTMQueue'. For debugging purposes.
_sourceBufferDebug :: B.Queue -> ConduitT () Int (ResourceT IO) ()
_sourceBufferDebug b =
  loop
  where
    loop = do
      mx <- liftIO . atomically $ readTMQueue b
      liftIO $ putStrLn $ "READ VALUE FROM BUFFER: " <> show mx
      case mx of
        Nothing -> pure ()
        Just x -> yield x >> loop
