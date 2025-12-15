{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module JbeamEdit.LSP.Server (runServer) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Control.Monad.IO.Class
import Data.Kind (Type)
import JbeamEdit.Formatting.Rules (RuleSet)
import JbeamEdit.LSP.Handlers.Formatting qualified as Formatting
import JbeamEdit.LSP.Services.DocumentStore qualified as Docs
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types qualified as J (
  DidChangeTextDocumentParams (..),
  DidCloseTextDocumentParams (..),
  DidOpenTextDocumentParams (..),
  TextDocumentContentChangeEvent (..),
  TextDocumentContentChangePartial (..),
  TextDocumentContentChangeWholeDocument (..),
  TextDocumentIdentifier (..),
  TextDocumentItem (..),
  TextDocumentSyncKind (..),
  TextDocumentSyncOptions (..),
  VersionedTextDocumentIdentifier (..),
  type (|?) (..),
 )
import Language.LSP.Server qualified as S
import System.IO (hPutStrLn, stderr)

logInfo :: MonadIO m => LogAction IO (WithSeverity String) -> String -> m ()
logInfo la msg =
  liftIO $ unLogAction la (WithSeverity msg Info)

staticHandlers
  :: RuleSet -> LogAction IO (WithSeverity String) -> S.Handlers (S.LspM config)
staticHandlers rs logAction =
  mconcat
    [ S.notificationHandler Msg.SMethod_Initialized $ \_ ->
        logInfo logAction "Client initialized"
    , S.notificationHandler Msg.SMethod_WorkspaceDidChangeConfiguration $ \_ ->
        logInfo logAction "Configuration changed"
    , S.notificationHandler Msg.SMethod_TextDocumentDidOpen $
        handleDidOpen logAction
    , S.notificationHandler Msg.SMethod_TextDocumentDidClose $
        handleDidClose logAction
    , S.notificationHandler Msg.SMethod_TextDocumentDidChange $
        handleDidChange logAction
    ]
    <> Formatting.handlers rs logAction

stderrLogger :: LogAction IO (WithSeverity String)
stderrLogger =
  LogAction $ \(WithSeverity msg sev) ->
    hPutStrLn stderr (show sev <> ": " <> msg)

-- | Starta LSP-servern
runServer :: RuleSet -> IO Int
runServer rs =
  S.runServer $
    S.ServerDefinition
      { configSection = "jbeam-lsp"
      , parseConfig = \_ _ -> Right ()
      , onConfigChange = const >> pure $ pure ()
      , defaultConfig = ()
      , doInitialize = \env _ -> pure (Right env)
      , staticHandlers = const $ staticHandlers rs stderrLogger
      , interpretHandler = \env -> S.Iso (S.runLspT env) liftIO
      , options =
          S.defaultOptions
            { S.optTextDocumentSync =
                Just
                  ( J.TextDocumentSyncOptions
                      { J._openClose = Just True
                      , J._change = Just J.TextDocumentSyncKind_Full
                      , J._willSave = Nothing
                      , J._willSaveWaitUntil = Nothing
                      , J._save = Nothing
                      }
                  )
            }
      }

-- | didOpen: save docuement in DocumentStore
handleDidOpen
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . ( MonadIO m2
     , Msg.MessageParams m1 ~ J.DidOpenTextDocumentParams
     )
  => LogAction IO (WithSeverity String)
  -> Msg.TNotificationMessage m1
  -> m2 ()
handleDidOpen
  logAction
  (Msg.TNotificationMessage _ _ (J.DidOpenTextDocumentParams textDoc)) =
    let J.TextDocumentItem {J._uri = uri, J._text = txt} = textDoc
     in liftIO $ do
          Docs.open uri txt
          logInfo logAction ("Document opened: " <> show uri)

-- | didChange: update document in DocumentStore
handleDidChange
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . ( MonadIO m2
     , Msg.MessageParams m1 ~ J.DidChangeTextDocumentParams
     )
  => LogAction IO (WithSeverity String)
  -> Msg.TNotificationMessage m1
  -> m2 ()
handleDidChange
  logAction
  (Msg.TNotificationMessage _ _ (J.DidChangeTextDocumentParams docId changes)) =
    let J.VersionedTextDocumentIdentifier {_uri = uri} = docId
     in liftIO $
          case changes of
            (J.TextDocumentContentChangeEvent change : _) -> do
              case change of
                J.InL (J.TextDocumentContentChangePartial {J._text = txt}) ->
                  Docs.update uri txt
                J.InR (J.TextDocumentContentChangeWholeDocument txt) ->
                  Docs.update uri txt

              logInfo logAction ("Document changed: " <> show uri)
            _ -> pure ()

handleDidClose
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . ( MonadIO m2
     , Msg.MessageParams m1 ~ J.DidCloseTextDocumentParams
     )
  => LogAction IO (WithSeverity String)
  -> Msg.TNotificationMessage m1
  -> m2 ()
handleDidClose
  logAction
  (Msg.TNotificationMessage _ _ (J.DidCloseTextDocumentParams docId)) =
    let J.TextDocumentIdentifier {_uri = uri} = docId
     in liftIO $ do
          Docs.delete uri
          logInfo  logAction ("Document closed: " <> show uri)
