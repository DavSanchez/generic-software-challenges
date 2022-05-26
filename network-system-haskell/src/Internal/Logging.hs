-- | Contains functions for basic destination file management.
-- The functions are defined in "Internal.Logging"
--
-- === Comments
-- Not much to say here, this module is short and its main use in the current version is to create the destination file and cleaning it up if it exists.
module Internal.Logging (cleanupFile, logValue) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

-- | Manually append an 'Int' value to a file. Used by "Internal.Consumer"'s 'C._consumeAndProcess'' function.
logValue :: FilePath -> Int -> IO ()
logValue f v = BS.appendFile f (valueWithNewline v)
  where
    valueWithNewline v' = C.pack $ show v' <> "\n"

-- | Delete the contents of a file.
cleanupFile :: FilePath -> IO ()
cleanupFile f = writeFile f []