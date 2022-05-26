{-# LANGUAGE DerivingStrategies #-}

-- | Module without exported functions (i.e. library only). Contains functions for creating, manipulating and outputting the 10-second report to console.
--
-- === Comments
-- This is a simple module. Versatility and robustness can be achieved by using actual logging functionality (with a logging library such as [co-log](https://hackage.haskell.org/package/co-log)) instead of the easy printing to @stdout@. However, given the time, I preferred to dedicate my efforts to elsewhere in the application architecture.
module Internal.Report
  ( -- * Types
    Report,
    ReportData (..),

    -- * Report printing
    printReport,

    -- * 'ReportData' manipulation
    createReport,
    addDuplicate,
    addUnique,
    addUniqueTotal,
    addUniqueAndTotal,
    resetReport,

    -- * STM functions for 'Report' manipulation
    reportOnNew,
    reportOnDuplicate,
    reportRunReset,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVarIO, readTVar)
import Control.Monad (forever)

-- | Abstract type representing a report.
data ReportData = ReportData
  { -- | Number of unique files inserted since the last report was printed.
    uniques :: Int,
    -- | Number of duplicates sent by clients since the last report was printed.
    duplicates :: Int,
    -- | Total of unique numbers sent by clients on this application run.
    uniqueTotal :: Int
  }
  deriving stock (Show, Eq)

-- | Report enclosed in a transactional variable ('TVar') for concurrent manipulation.
type Report = TVar ReportData

-- | Prints the current 'ReportData' values every 10 seconds. Resets the 'uniques' and 'duplicates' fields of the report.
printReport :: Report -> IO ()
printReport report = forever $ do
  threadDelay $ 10 * 1000 * 1000
  r <- atomically $ do
    currentVal <- readTVar report
    reportRunReset report
    pure currentVal
  putStrLn $
    "Received " <> (show . uniques) r <> " unique numbers, "
      <> (show . duplicates) r
      <> " duplicates. "
      <> "Unique total: "
      <> (show . uniqueTotal) r

-- | Creates a new, empty 'Report'.
createReport :: IO Report
createReport =
  newTVarIO
    ReportData
      { uniques = 0,
        duplicates = 0,
        uniqueTotal = 0
      }

-- |  Adds a duplicate occurrence to the report (essentially, summing @1@ to the current value).
--
--  >>> addDuplicate (ReportData 0 0 0)
--  ReportData {uniques = 0, duplicates = 1, uniqueTotal = 0}
addDuplicate :: ReportData -> ReportData
addDuplicate r@(ReportData _ d _) = r {duplicates = succ d}

-- |  Adds an unique number occurrence to the report (essentially, summing @1@ to the current value).
--
--  >>> addUnique (ReportData 0 0 0)
--  ReportData {uniques = 1, duplicates = 0, uniqueTotal = 0}
addUnique :: ReportData -> ReportData
addUnique r@(ReportData u _ _) = r {uniques = succ u}

-- |  Adds a unique number to the total report (essentially, summing @1@ to the current value).
--
--  >>> addUniqueTotal (ReportData 0 0 0)
--  ReportData {uniques = 0, duplicates = 0, uniqueTotal = 1}
addUniqueTotal :: ReportData -> ReportData
addUniqueTotal r@(ReportData _ _ t) = r {uniqueTotal = succ t}

-- | 'addUnique' and 'addUniqueTotal' go together, so this function is the composition of both.
--
-- >>> addUniqueAndTotal (ReportData 0 0 0)
-- ReportData {uniques = 1, duplicates = 0, uniqueTotal = 1}
addUniqueAndTotal :: ReportData -> ReportData
addUniqueAndTotal = addUniqueTotal . addUnique

-- | Resets the report, setting 'uniques' and 'duplicates' to @0@. Called every 10 seconds by 'printReport'.
--
-- >>> resetReport (ReportData 1 2 3)
-- ReportData {uniques = 0, duplicates = 0, uniqueTotal = 3}
resetReport :: ReportData -> ReportData
resetReport r = r {uniques = 0, duplicates = 0}

-- | Updates the duplicates count on the 'TVar' 'Report'.
reportOnDuplicate :: Report -> STM ()
reportOnDuplicate r = modifyTVar r addDuplicate

-- | Updates the uniques count on the 'TVar' 'Report'.
reportOnNew :: Report -> STM ()
reportOnNew r = modifyTVar r addUniqueAndTotal

-- | Resets the report on the 'TVar' 'Report'.
reportRunReset :: Report -> STM ()
reportRunReset r = modifyTVar r resetReport
