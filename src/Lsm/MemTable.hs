module Lsm.MemTable where

-- use a STM based skip list for the memtable

import Control.Arrow (left)
import Control.Concurrent.STM.Map as STMMap
import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.Either qualified as Either
import Data.Foldable (foldr', foldrM)
import Data.Hashable (Hashable)
import Data.Map.Strict qualified as Map
import GHC.Conc (STM, atomically)
import Lsm.Error (LsmResult)
import Lsm.Wal (Wal, walRecover, walSync, walWrite)

{- |
A memtable is a data structure used to store the most recent key-value pairs
in memory.

It is a write-back cache that is periodically flushed to disk.
-}
data MemTable k v = MemTable
    { -- \| The skip list that stores the key-value pairs
      mtMap :: STMMap.Map k v
    , -- \| The file path to which the memtable will be flushed
      mtWal :: Wal
    }

mtInsert ::
    (Hashable k) =>
    (Ord k) =>
    (Binary k) =>
    (Binary v) =>
    MemTable k v ->
    k ->
    v ->
    IO ()
mtInsert (MemTable{mtMap, mtWal}) k v = do
    walWrite mtWal (encodeStrict k) (encodeStrict v)
    walSync mtWal
    atomically $ STMMap.insert k v mtMap
    where
        encodeStrict :: (Binary a) => a -> BS.ByteString
        encodeStrict = BSL.toStrict . B.encode

mtRecover ::
    (Ord k) =>
    (Hashable k) =>
    (Binary k) =>
    (Binary v) =>
    FilePath ->
    IO (LsmResult (MemTable k v))
mtRecover path = do
    res <- walRecover path
    case res of
        Left err -> return $ Left err
        Right (mtWal, pairs) -> do
            let lst = Map.toList pairs
            let lst' = listDecode lst
            mtMap <- STMMap.fromList lst'
            return $ Right $ MemTable{mtMap, mtWal}
            where
                listDecode ::
                    (Binary k, Binary v) => [(BS.ByteString, BS.ByteString)] -> [(k, v)]
                listDecode = map decodePair
                decodePair :: (Binary k, Binary v) => (BS.ByteString, BS.ByteString) -> (k, v)
                decodePair (k, v) = (decodeStrict k, decodeStrict v)
                decodeStrict :: (Binary a) => BS.ByteString -> a
                decodeStrict = B.decode . BSL.fromStrict
