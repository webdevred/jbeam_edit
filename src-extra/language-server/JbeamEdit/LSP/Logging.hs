module JbeamEdit.LSP.Logging (logDebug, logInfo, logWarning, logError) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T (unpack)

logMsg
  :: MonadIO m => Severity -> LogAction IO (WithSeverity String) -> Text -> m ()
logMsg sev logAction msg =
  let msgTxt = T.unpack msg
      msgWithSeverity = WithSeverity msgTxt sev
   in liftIO (unLogAction logAction msgWithSeverity)

logDebug :: MonadIO m => LogAction IO (WithSeverity String) -> Text -> m ()
logDebug = logMsg Debug

logInfo :: MonadIO m => LogAction IO (WithSeverity String) -> Text -> m ()
logInfo = logMsg Info

logWarning :: MonadIO m => LogAction IO (WithSeverity String) -> Text -> m ()
logWarning = logMsg Warning

logError :: MonadIO m => LogAction IO (WithSeverity String) -> Text -> m ()
logError = logMsg Error
