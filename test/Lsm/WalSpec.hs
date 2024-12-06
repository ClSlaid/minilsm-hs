{-# LANGUAGE OverloadedStrings #-}

module Lsm.WalSpec where

import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import Lsm.Wal
import System.Directory (removeFile)
import System.IO
import Test.Hspec

spec_wal :: Spec
spec_wal = do
    describe "Lsm.Wal" $ do
        before (removeFile "test0.wal" `catch` (\(_ :: IOException) -> return ())) $ do
            it "should recover a WAL file" $ do
                wal <- walCreate "test0.wal"
                walWrite wal "key1" "value1"
                walWrite wal "key2" "value2"
                walWrite wal "key3" "value3"
                walSync wal
                _ <- hClose (walHandler wal)
                result <- walRecover "test0.wal"
                case result of
                    Left err -> expectationFailure $ "Failed to recover WAL: " ++ show (err)
                    Right (_, pairs) ->
                        do
                            pairs `shouldBe` [("key1", "value1"), ("key2", "value2"), ("key3", "value3")]
        after
            (\_ -> (removeFile "test0.wal" `catch` (\(_ :: IOException) -> return ())))
            $ do
                return ()

    describe "Corrupted WAL" $ do
        before (removeFile "test1.wal" `catch` (\(_ :: IOException) -> return ())) $ do
            it "recover a corrupted WAL file" $ do
                wal <- walCreate "test1.wal"
                walWrite wal "key1" "value1"
                walWrite wal "key2" "value2"
                walWrite wal "key3" "value3"
                walSync wal
                _ <- hClose (walHandler wal)

                h <- openFile "test1.wal" WriteMode
                hSeek h AbsoluteSeek 8
                BSL.hPut h "V"
                _ <- hClose h

                result <- walRecover "test1.wal"
                -- should fail
                case result of
                    Left _ -> do True `shouldBe` True
                    Right _ -> expectationFailure $ "WAL recover should fail"
        after
            (\_ -> (removeFile "test0.wal" `catch` (\(_ :: IOException) -> return ())))
            $ do
                return ()
