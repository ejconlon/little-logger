{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main (main) where

import Control.Exception (finally)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import LittleLogger (LogAction (LogAction), Msg (msgSeverity, msgText), Severity (..), SimpleLogAction, WithSimpleLog,
                     fileSimpleLogAction, filterActionSeverity, logDebug, logError, logInfo, logWarning,
                     runWithSimpleLogAction)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

emitLogs :: WithSimpleLog env m => m ()
emitLogs = do
  logDebug "debug"
  logInfo "info"
  logWarning "warning"
  logError "error"

refAction :: IORef [(Severity, Text)] -> SimpleLogAction
refAction ref = LogAction (\msg -> modifyIORef' ref (++ [(msgSeverity msg, msgText msg)]))

runWithRefAction :: (SimpleLogAction -> SimpleLogAction) -> (forall env m. WithSimpleLog env m => m ()) -> IO [(Severity, Text)]
runWithRefAction f m = do
  ref <- newIORef []
  let action = f (refAction ref)
  runWithSimpleLogAction action m
  readIORef ref

expectedFiltered :: [(Severity, Text)]
expectedFiltered =
  [ (Warning, "warning")
  , (Error, "error")
  ]

expectedUnfiltered :: [(Severity, Text)]
expectedUnfiltered =
  [ (Debug, "debug")
  , (Info, "info")
  ] ++ expectedFiltered

testUnfiltered :: TestTree
testUnfiltered = testCase "Unfiltered" $ do
  actual <- runWithRefAction id emitLogs
  actual @?= expectedUnfiltered

testFiltered :: TestTree
testFiltered = testCase "Filtered" $ do
  actual <- runWithRefAction (filterActionSeverity Warning) emitLogs
  actual @?= expectedFiltered

testFile :: TestTree
testFile = testCase "File" $ do
  fp <- emptySystemTempFile "little-logger-test"
  flip finally (removeFile fp) $ do
    fileSimpleLogAction fp (`runWithSimpleLogAction` emitLogs)
    firstContents <- readFile fp
    length (lines firstContents) @?= 4
    fileSimpleLogAction fp (`runWithSimpleLogAction` emitLogs)
    secondContents <- readFile fp
    length (lines secondContents) @?= 8

main :: IO ()
main = defaultMain $ testGroup "LittleLogger" $
  [ testUnfiltered
  , testFiltered
  , testFile
  ]
