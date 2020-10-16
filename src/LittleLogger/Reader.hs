{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

-- | A logging "backend" where you get the log action from the environment.
module LittleLogger.Reader
  ( module LittleLogger.Common
  , HasSimpleLog (..)
  , WithSimpleLog
  , logMsg
  , logDebug
  , logError
  , logException
  , logInfo
  , logWarning
  , logWithSeverity
  , runWithSimpleLogAction
  ) where

import Control.Exception (Exception, displayException)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Lens.Micro (Lens')
import Lens.Micro.Extras (view)
import LittleLogger.Common

class HasSimpleLog env where
  simpleLogL :: Lens' env SimpleLogAction

instance HasSimpleLog SimpleLogAction where
  simpleLogL = id

type WithSimpleLog env m = (MonadIO m, MonadReader env m, HasSimpleLog env, HasCallStack)

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

-- | Usually 'm' will be some kind of 'Reader' monad. In the case where you don't care what it
-- is and you only need to do logging and IO, you can use this.
runWithSimpleLogAction :: SimpleLogAction -> (forall env m. WithSimpleLog env m => m a) -> IO a
runWithSimpleLogAction = flip runReaderT
