module Lsm.MemTable where

-- use a STM based skip list for the memtable
import Control.Concurrent.STM.TSkipList as TSL
import GHC.Conc (STM)

{- |
A memtable is a data structure used to store the most recent key-value pairs
in memory.

It is a write-back cache that is periodically flushed to disk.
-}
data MemTable k v = MemTable
    { -- \| The skip list that stores the key-value pairs
      skipList :: TSkipList k v
    , -- \| The file path to which the memtable will be flushed
      wal :: FilePath
    }

mtInsert :: (Ord k) => MemTable k v -> k -> v -> STM ()
mtInsert (MemTable{skipList}) k v = TSL.insert k v skipList


