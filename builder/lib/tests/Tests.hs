{-# LANGUAGE OverloadedStrings #-}

module Main where

import Site
import qualified System.Exit as Exit
import Test.HUnit

testCopy :: Test
testCopy = TestCase $ assert True
  where
    test = do
      copy "some-file"
      copy ["some", "file"]

tests :: Test
tests = TestList [TestLabel "testCopy" testCopy]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
