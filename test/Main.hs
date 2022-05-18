{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (finally)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import LittleLogger (LogAction (..), LogLevel (..), LogActionM, runLogActionM, textLogStr,
                     fileLogAction, filterActionSeverity, logDebugN, logErrorN, logInfoN, logWarnN)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

emitLogs :: LogActionM ()
emitLogs = do
  logDebugN "debug"
  logInfoN "info"
  logWarnN "warn"
  logErrorN "error"

refAction :: IORef [(LogLevel, Text)] -> LogAction
refAction ref = LogAction (\_ _ lvl msg -> modifyIORef' ref (++ [(lvl, textLogStr msg)]))

runWithRefAction :: (LogAction -> LogAction) -> (LogActionM ()) -> IO [(LogLevel, Text)]
runWithRefAction f m = do
  ref <- newIORef []
  let action = f (refAction ref)
  runLogActionM m action
  readIORef ref

expectedFiltered :: [(LogLevel, Text)]
expectedFiltered =
  [ (LevelWarn, "warn")
  , (LevelError, "error")
  ]

expectedUnfiltered :: [(LogLevel, Text)]
expectedUnfiltered =
  [ (LevelDebug, "debug")
  , (LevelInfo, "info")
  ] ++ expectedFiltered

testUnfiltered :: TestTree
testUnfiltered = testCase "Unfiltered" $ do
  actual <- runWithRefAction id emitLogs
  actual @?= expectedUnfiltered

testFiltered :: TestTree
testFiltered = testCase "Filtered" $ do
  actual <- runWithRefAction (filterActionSeverity LevelWarn) emitLogs
  actual @?= expectedFiltered

testFile :: TestTree
testFile = testCase "File" $ do
  fp <- emptySystemTempFile "little-logger-test"
  flip finally (removeFile fp) $ do
    fileLogAction fp (runLogActionM emitLogs)
    firstContents <- readFile fp
    length (lines firstContents) @?= 4
    fileLogAction fp (runLogActionM emitLogs)
    secondContents <- readFile fp
    length (lines secondContents) @?= 8

main :: IO ()
main = defaultMain $ testGroup "LittleLogger" $
  [ testUnfiltered
  , testFiltered
  , testFile
  ]
