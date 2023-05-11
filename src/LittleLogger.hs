module LittleLogger
  ( textLogStr
  , LogAction (..)
  , defaultLogAction
  , filterActionSeverity
  , newLogAction
  , runLogAction
  , handleLogAction
  , openLoggingHandle
  , closeLoggingHandle
  , fileLogAction
  , HasLogAction (..)
  , WithLogAction
  , askLogAction
  , LogActionWrapperM (..)
  , LogActionT (..)
  , runLogActionT
  , LogActionM
  , runLogActionM
  -- Re-exports
  , MonadLogger (..)
  , Loc (..)
  , LogSource
  , LogLevel (..)
  , LogStr
  , ToLogStr (..)
  , logDebugN
  , logInfoN
  , logWarnN
  , logErrorN
  , logOtherN
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, askRunInIO)
import Control.Monad.Logger.CallStack
  ( Loc (..)
  , LogLevel (..)
  , LogSource
  , LogStr
  , MonadLogger (..)
  , ToLogStr (..)
  , defaultOutput
  , fromLogStr
  , logDebugN
  , logErrorN
  , logInfoN
  , logOtherN
  , logWarnN
  )
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro (Lens')
import Lens.Micro.Extras (view)
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , IOMode (AppendMode)
  , hClose
  , hSetBuffering
  , openFile
  , stderr
  , withFile
  )

textLogStr :: LogStr -> Text
textLogStr = decodeUtf8 . fromLogStr

newtype LogAction = LogAction {unLogAction :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()}

instance Semigroup LogAction where
  LogAction act1 <> LogAction act2 = LogAction (\loc src lvl msg -> act1 loc src lvl msg >> act2 loc src lvl msg)

instance Monoid LogAction where
  mempty = LogAction (\_ _ _ _ -> pure ())
  mappend = (<>)

newLogAction :: MonadUnliftIO m => (Loc -> LogSource -> LogLevel -> LogStr -> m ()) -> m LogAction
newLogAction act = fmap (\run -> LogAction (\loc src lvl msg -> run (act loc src lvl msg))) askRunInIO

runLogAction :: MonadIO m => LogAction -> Loc -> LogSource -> LogLevel -> LogStr -> m ()
runLogAction (LogAction act) loc src lvl msg = liftIO (act loc src lvl msg)

defaultLogAction :: LogAction
defaultLogAction = LogAction (defaultOutput stderr)

filterActionSeverity :: LogLevel -> LogAction -> LogAction
filterActionSeverity lim (LogAction act) =
  LogAction (\loc src lvl msg -> when (lvl >= lim) (act loc src lvl msg))

handleLogAction :: Handle -> LogAction
handleLogAction = LogAction . defaultOutput

openLoggingHandle :: MonadIO m => FilePath -> m Handle
openLoggingHandle fp = do
  handle <- liftIO (openFile fp AppendMode)
  liftIO (hSetBuffering handle LineBuffering)
  pure handle

closeLoggingHandle :: MonadIO m => Handle -> m ()
closeLoggingHandle = liftIO . hClose

fileLogAction :: MonadUnliftIO m => FilePath -> (LogAction -> m a) -> m a
fileLogAction fp f = do
  run <- askRunInIO
  liftIO $ withFile fp AppendMode $ \handle -> do
    hSetBuffering handle LineBuffering
    run (f (handleLogAction handle))

class HasLogAction env where
  logActionL :: Lens' env LogAction

instance HasLogAction LogAction where
  logActionL = id

type WithLogAction env m = (MonadIO m, MonadReader env m, HasLogAction env)

askLogAction :: (MonadReader env m, HasLogAction env) => m LogAction
askLogAction = asks (view logActionL)

-- | Use deriving-via with this wrapper to add MonadLogger instances to your types
newtype LogActionWrapperM env m a = LogActionM {unLogActionM :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env)

instance WithLogAction env m => MonadLogger (LogActionWrapperM env m) where
  monadLoggerLog loc src lvl msg = do
    LogAction act <- askLogAction
    liftIO (act loc src lvl (toLogStr msg))

newtype LogActionT m a = LogActionT {unLogActionT :: ReaderT LogAction m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader LogAction)
  deriving (MonadLogger) via (LogActionWrapperM LogAction (LogActionT m))

runLogActionT :: LogActionT m a -> LogAction -> m a
runLogActionT = runReaderT . unLogActionT

type LogActionM a = LogActionT IO a

runLogActionM :: LogActionM a -> LogAction -> IO a
runLogActionM = runLogActionT
