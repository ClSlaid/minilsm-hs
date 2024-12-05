{-# LANGUAGE OverloadedStrings #-}

module Lsm.WalSpec where

import Control.Exception
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
            (\_ -> (removeFile "test0.wal" `catch` (\(_ :: IOException) -> return ()))) $ do
            return ()
