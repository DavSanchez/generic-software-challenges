-- | This is the interface module for the functions related to the consumer, that is,
-- the element that reads from the 'B.Queue' exposed by "Buffers" and processes its elements in a streaming pipeline.
-- The exported function 'consumeAndProcess' is explained in "Internal.Consumer".
module Consumer (consumeAndProcess) where

import Internal.Consumer