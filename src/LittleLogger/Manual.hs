-- | A logging "backend" where you pass around the log action manually.
module LittleLogger.Manual
  ( module LittleLogger.Common
  , logMsg
  , logDebug
  , logError
  , logException
  , logInfo
  , logWarning
  , logWithSeverity
  ) where

import Control.Exception (Exception, displayException)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import LittleLogger.Common

logMsg :: MonadIO m => SimpleLogAction -> Message -> m ()
logMsg = runSimpleLogAction

logWithSeverity :: (MonadIO m, HasCallStack) => SimpleLogAction -> Severity -> Text -> m ()
logWithSeverity act sev txt = withFrozenCallStack (logMsg act Msg { msgStack = callStack, msgSeverity = sev, msgText = txt })

logDebug :: (MonadIO m, HasCallStack) => SimpleLogAction -> Text -> m ()
logDebug act = withFrozenCallStack (logWithSeverity act Debug)

logInfo :: (MonadIO m, HasCallStack) => SimpleLogAction -> Text -> m ()
logInfo act = withFrozenCallStack (logWithSeverity act Info)

logWarning :: (MonadIO m, HasCallStack) => SimpleLogAction -> Text -> m ()
logWarning act = withFrozenCallStack (logWithSeverity act Warning)

logError :: (MonadIO m, HasCallStack) => SimpleLogAction -> Text -> m ()
logError act = withFrozenCallStack (logWithSeverity act Error)

logException :: (MonadIO m, Exception e, HasCallStack) => SimpleLogAction -> e -> m ()
logException act = withFrozenCallStack (logError act . Text.pack . displayException)
