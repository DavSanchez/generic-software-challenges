-- | This is the interface module that exposes the functions related to channels and buffers, that is,
-- the channel to which the Server writes and from which the Consumer reads, and also the set of currently stored 'Int's
-- that the Consumer compares with the incoming 'Int's. The exported functions are explained in "Internal.Buffers".
module Buffers (Queue, InsertedStore, createQueue, insertInQueue, closeQueue, createInsertedStore, isDuplicate, addElement) where

import Internal.Buffers