{-# LANGUAGE ConstraintKinds #-}

{-| Basic logging based on co-log. The difference is that our log action runs in IO, and
  we expect to use it in any IO-lifting monad. -}
module LittleLogger
  ( LogApp (..)
  , SimpleLogAction
  , HasSimpleLog (..)
  , WithSimpleLog
  , logAppLogAction
  , defaultSimpleLogAction
  , logMsg
  , logDebug
  , logError
  , logException
  , logInfo
  , logWarning
  , logWithSeverity
  , newLogApp
  ) where

import Colog.Actions (richMessageAction)
import Colog.Core.Action (LogAction (..))
import Colog.Core.Severity (Severity (..))
import Colog.Message (Message, Msg (..))
import Control.Exception (Exception, displayException)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Lens.Micro (Lens', lens)
import Lens.Micro.Extras (view)
import Prelude

type SimpleLogAction = LogAction IO Message

class HasSimpleLog env where
  simpleLogL :: Lens' env SimpleLogAction

type WithSimpleLog env m = (MonadIO m, MonadReader env m, HasSimpleLog env, HasCallStack)

defaultSimpleLogAction :: SimpleLogAction
defaultSimpleLogAction = richMessageAction

logMsg :: WithSimpleLog env m => Message -> m ()
logMsg msg = do
  env <- ask
  let LogAction act = view simpleLogL env
  liftIO (act msg)

logWithSeverity :: WithSimpleLog env m => Severity -> Text -> m ()
logWithSeverity sev txt = withFrozenCallStack (logMsg Msg { msgStack = callStack, msgSeverity = sev, msgText = txt })

logDebug :: WithSimpleLog env m => Text -> m ()
logDebug = withFrozenCallStack (logWithSeverity Debug)

logInfo :: WithSimpleLog env m => Text -> m ()
logInfo = withFrozenCallStack (logWithSeverity Info)

logWarning :: WithSimpleLog env m => Text -> m ()
logWarning = withFrozenCallStack (logWithSeverity Warning)

logError :: WithSimpleLog env m => Text -> m ()
logError = withFrozenCallStack (logWithSeverity Error)

logException :: (WithSimpleLog env m, Exception e) => e -> m ()
logException = withFrozenCallStack (logError . Text.pack . displayException)

newtype LogApp = LogApp { _logAppLogAction :: SimpleLogAction }

logAppLogAction :: Lens' LogApp SimpleLogAction
logAppLogAction = lens _logAppLogAction (const LogApp)

instance HasSimpleLog LogApp where
  simpleLogL = logAppLogAction

newLogApp :: LogApp
newLogApp = LogApp defaultSimpleLogAction
