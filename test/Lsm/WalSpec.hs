{-# LANGUAGE OverloadedStrings #-}

module Lsm.WalSpec where

import Control.Exception
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Lsm.Wal
import System.Directory (removeFile)
import System.IO
import Test.Hspec

spec_wal :: Spec
spec_wal = do
    describe "Lsm.Wal" $ do
        before (removeFile "test0.wal" `catch` (\(_ :: IOException) -> return ())) $ it "should recover a WAL file" $ do
            wal <- walCreate "test0.wal"
            walWrite wal "key1" "value1"
            walWrite wal "key2" "value2"
            walWrite wal "key3" "old_value3"
            walWrite wal "key3" "value3"
            walSync wal
            _ <- hClose (walHandler wal)
            result <- walRecover "test0.wal"
            case result of
                Left err -> expectationFailure $ "Failed to recover WAL: " ++ show err
                Right (_, pairs) ->
                    do
                        Map.lookup "key1" pairs `shouldBe` Just "value1"
                        Map.lookup "key2" pairs `shouldBe` Just "value2"
                        Map.lookup "key3" pairs `shouldBe` Just "value3"
        after
            ( const
                ( removeFile "test0.wal"
                    `catch` (\(_ :: IOException) -> return ())
                )
            )
            $ return ()

    describe "Corrupted WAL" $ do
        before (removeFile "test1.wal" `catch` (\(_ :: IOException) -> return ())) $ it "recover a corrupted WAL file" $ do
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
                Left _ -> True `shouldBe` True
                Right _ -> expectationFailure "WAL recover should fail"
        after
            ( const
                ( removeFile "test0.wal"
                    `catch` (\(_ :: IOException) -> return ())
                )
            )
            $ return ()
    describe "Recover and Recover test" $ do
        before (removeFile "test2.wal" `catch` (\(_ :: IOException) -> return ())) $ it "should recover a WAL file and continue writing" $ do
            wal <- walCreate "test2.wal"
            walWrite wal "key1" "value1"
            walWrite wal "key2" "value2"
            walWrite wal "key3" "old_value3"
            walSync wal
            _ <- hClose (walHandler wal)
            result1 <- walRecover "test2.wal"
            case result1 of
                Left err -> expectationFailure $ "Failed to recover WAL: " ++ show err
                Right (wal', pairs1) ->
                    do
                        Map.lookup "key1" pairs1 `shouldBe` Just "value1"
                        Map.lookup "key2" pairs1 `shouldBe` Just "value2"
                        Map.lookup "key3" pairs1 `shouldBe` Just "old_value3"
                        -- Continue writing to the recovered WAL
                        walWrite wal' "key3" "value3"
                        walWrite wal' "key4" "value4"
                        walWrite wal' "key5" "value5"
                        walSync wal'
                        _ <- hClose (walHandler wal')
                        -- Recover again and check the results
                        result2 <- walRecover "test2.wal"
                        case result2 of
                            Left err -> expectationFailure $ "Failed to recover WAL: " ++ show err
                            Right (_, pairs2) ->
                                do
                                    Map.lookup "key1" pairs2 `shouldBe` Just "value1"
                                    Map.lookup "key2" pairs2 `shouldBe` Just "value2"
                                    Map.lookup "key3" pairs2 `shouldBe` Just "value3"
                                    Map.lookup "key4" pairs2 `shouldBe` Just "value4"
                                    Map.lookup "key5" pairs2 `shouldBe` Just "value5"
        after
            ( const
                ( removeFile "test2.wal"
                    `catch` (\(_ :: IOException) -> return ())
                )
            )
            $ return ()
