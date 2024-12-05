{- |
WAL stands for Write-Ahead Log. It is a data structure used to store the most recent key-value pairs on disk.

The on disk WAL format is a sequence of key-value pairs, each pair is stored in the following format:

+-------------------------------------+
|    16 bits key size (Big Endian)    |
+-------------------------------------+
|             key content             |
+-------------------------------------+
|   16 bits value size (Big Endian)   |
+-------------------------------------+
|           value content             |
+-------------------------------------+
| 32 bits CRC32 checksum (Big Endian) |
+-------------------------------------+
-}
module Lsm.Wal (
    Wal(..),
    walCreate,
    walRecover,
    walWrite,
    walSync,
) where

import Control.Exception (try)
import Data.Binary qualified as B
import Data.Binary.Get (
    bytesRead,
    getByteString,
    getWord16be,
    getWord32be,
    runGetOrFail,
 )
import Data.Binary.Put (putByteString, putWord16be, putWord32be, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (hGet)
import Data.ByteString.Lazy qualified as BSL
import Data.Digest.CRC32 (crc32, crc32Update)
import Data.Word (Word16, Word32)
import GHC.IO.Handle (hFlushAll)
import Lsm.Error (LsmError (..), LsmErrorType (..), LsmResult)
import System.IO (
    Handle,
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    hClose,
    hGetContents,
    hPutStr,
    openFile,
 )

type WalList = [(BS.ByteString, BS.ByteString)]

{- | A write-ahead log (WAL) is a data structure used to store the most recent
key-value pairs on disk.
-}
newtype Wal = Wal
    { walHandler :: Handle
    }

-- | createWal creates a new WAL file at the given path.
walCreate :: FilePath -> IO Wal
walCreate path = do
    walHandler <- openFile path AppendMode
    return Wal{walHandler}

-- | recoverPair reads the WAL file at the given path and returns a key-value pair.
getPair :: B.Get (LsmResult (BS.ByteString, BS.ByteString))
getPair = do
    keySize <- getWord16be
    key <- getByteString (fromIntegral keySize)
    valueSize <- getWord16be
    value <- getByteString (fromIntegral valueSize)
    checksum <- getWord32be

    currentPos <- bytesRead

    let crc32kl = crc32 (runPut (putWord16be keySize))
    let crc32k = crc32Update crc32kl key
    let crc32kvl = crc32Update crc32k (runPut (putWord16be valueSize))
    let actualChecksum = crc32Update crc32kvl value

    return $
        if checksum /= actualChecksum
            then
                let beginPos = currentPos - 8 - fromIntegral (keySize + valueSize)
                 in Left $
                        LsmError
                            { lsmErrType =
                                IoErr
                                    { ioErr = userError "Checksum mismatch"
                                    , ioErrInfo =
                                        "Checksum mismatch at " <> show beginPos <> "-" <> show currentPos
                                    }
                            }
            else Right (key, value)

putPair :: (BS.ByteString, BS.ByteString) -> B.Put
putPair (key, value) = do
    let keySize = fromIntegral $ BS.length key :: Word16
    let valueSize = fromIntegral $ BS.length value :: Word16

    let crc32kl = crc32 (runPut (putWord16be keySize))
    let crc32k = crc32Update crc32kl key
    let crc32kvl = crc32Update crc32k (runPut (putWord16be valueSize))
    let crc32sum = crc32Update crc32kvl value

    putWord16be (fromIntegral keySize)
    putByteString key
    putWord16be (fromIntegral valueSize)
    putByteString value
    putWord32be crc32sum

-- | recoverList reads or create the WAL file at the given path and returns a list of key-value pairs.
recoverList :: FilePath -> IO (LsmResult WalList)
recoverList path = do
    handler <- openFile path ReadWriteMode
    content <- BSL.hGetContents handler
    -- run getPair on the content till the end
    return $ parsePairs content
    where
        parsePairs :: BSL.ByteString -> LsmResult WalList
        parsePairs bs = go bs []
            where
                go remaining pairs =
                    if BSL.null remaining
                        then Right pairs
                        else case runGetOrFail getPair remaining of
                            Left (_, _, err) -> Left $ LsmError (Other err)
                            Right (remaining', _, Right pair) -> go remaining' (pairs <> [pair])
                            Right (_, _, Left err) -> Left err

-- | recoverWal reads the WAL file at the given path and returns a WAL instance and a list of key-value pairs.
walRecover :: FilePath -> IO (LsmResult (Wal, WalList))
walRecover path = do
    walList <- recoverList path
    case walList of
        Left err -> return $ Left err
        Right pairs -> do
            walHandler <- openFile path AppendMode
            return $ Right (Wal{walHandler}, pairs)

-- | writeWal writes the given key-value pair to the WAL file.
walWrite :: Wal -> BS.ByteString -> BS.ByteString -> IO ()
walWrite wal key value = do
    let content = runPut (putPair (key, value))
    BSL.hPutStr (walHandler wal) content

-- | writeBatchWal writes the given key-value pairs to the WAL file.
walWriteBatch :: Wal -> WalList -> IO ()
walWriteBatch = error "TODO"

-- | flushWal flushes the WAL file.
walSync :: Wal -> IO ()
walSync wal = hFlushAll (walHandler wal)
