module LittleLogger.Common
  ( LogAction (..)
  , Message
  , Msg (..)
  , Severity (..)
  , SimpleLogAction
  , defaultSimpleLogAction
  , filterActionSeverity
  , newSimpleLogAction
  , runSimpleLogAction
  ) where

import Colog.Actions (richMessageAction)
import Colog.Core.Action (LogAction (..))
import Colog.Core.Severity (Severity (..))
import Colog.Message (Message, Msg (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)

type SimpleLogAction = LogAction IO Message

newSimpleLogAction :: MonadUnliftIO m => (Message -> m ()) -> m SimpleLogAction
newSimpleLogAction f = fmap (\run -> LogAction (run . f)) askRunInIO

runSimpleLogAction :: MonadIO m => SimpleLogAction -> Message -> m ()
runSimpleLogAction (LogAction actIO) = liftIO . actIO

defaultSimpleLogAction :: SimpleLogAction
defaultSimpleLogAction = richMessageAction

filterActionSeverity :: Severity -> SimpleLogAction -> SimpleLogAction
filterActionSeverity lim (LogAction f) = LogAction (\msg -> if msgSeverity msg >= lim then f msg else pure ())
