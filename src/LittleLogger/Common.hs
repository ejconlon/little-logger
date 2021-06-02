module LittleLogger.Common
  ( LogAction (..)
  , Message
  , Msg (..)
  , Severity (..)
  , SimpleLogAction
  , defaultSimpleLogAction
  , filterActionSeverity
  , newSimpleLogAction
  , runLogAction
  , runSimpleLogAction
  , handleSimpleLogAction
  , openLoggingHandle
  , closeLoggingHandle
  , fileSimpleLogAction
  ) where

import Colog.Actions (logByteStringHandle, richMessageAction)
import Colog.Core.Action (LogAction (..), cmapM)
import Colog.Core.Severity (Severity (..))
import Colog.Message (Message, Msg (..), defaultFieldMap, fmtRichMessageDefault, upgradeMessageAction)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Data.Text.Encoding (encodeUtf8)
import System.IO (BufferMode (LineBuffering), Handle, IOMode (AppendMode), hClose, hSetBuffering, openFile, withFile)

type SimpleLogAction = LogAction IO Message

newSimpleLogAction :: MonadUnliftIO m => (Message -> m ()) -> m SimpleLogAction
newSimpleLogAction f = fmap (\run -> LogAction (run . f)) askRunInIO

runSimpleLogAction :: MonadIO m => SimpleLogAction -> Message -> m ()
runSimpleLogAction = runLogAction

runLogAction :: MonadIO m => LogAction IO msg -> msg -> m ()
runLogAction (LogAction actIO) = liftIO . actIO

defaultSimpleLogAction :: SimpleLogAction
defaultSimpleLogAction = richMessageAction

filterActionSeverity :: Severity -> SimpleLogAction -> SimpleLogAction
filterActionSeverity lim (LogAction f) = LogAction (\msg -> if msgSeverity msg >= lim then f msg else pure ())

handleSimpleLogAction :: Handle -> SimpleLogAction
handleSimpleLogAction handle =  upgradeMessageAction defaultFieldMap $
    cmapM (fmap encodeUtf8 . fmtRichMessageDefault) (logByteStringHandle handle)

openLoggingHandle :: MonadIO m => FilePath -> m Handle
openLoggingHandle fp = do
  handle <- liftIO (openFile fp AppendMode)
  liftIO (hSetBuffering handle LineBuffering)
  pure handle

closeLoggingHandle :: MonadIO m => Handle -> m ()
closeLoggingHandle = liftIO . hClose

fileSimpleLogAction :: MonadUnliftIO m => FilePath -> (SimpleLogAction -> m a) -> m a
fileSimpleLogAction fp f = do
  run <- askRunInIO
  liftIO $ withFile fp AppendMode $ \handle -> do
    hSetBuffering handle LineBuffering
    run (f (handleSimpleLogAction handle))
