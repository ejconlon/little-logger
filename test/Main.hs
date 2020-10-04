{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main (main) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import LittleLogger (LogAction (LogAction), Msg (msgSeverity, msgText), Severity (..), SimpleLogAction, WithSimpleLog,
                     filterActionSeverity, logDebug, logError, logInfo, logWarning, runWithSimpleLogAction)
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

main :: IO ()
main = defaultMain $ testGroup "LittleLogger" $
  [ testUnfiltered
  , testFiltered
  ]
