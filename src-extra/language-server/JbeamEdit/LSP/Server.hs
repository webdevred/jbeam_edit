{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module JbeamEdit.LSP.Server (runServer) where

import Colog.Core
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.Text (Text)
import JbeamEdit.Formatting.Rules (RuleSet)
import JbeamEdit.LSP.Handlers.Formatting qualified as Formatting
import JbeamEdit.LSP.Services.DocumentStore qualified as Docs
import Language.LSP.Logging
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.Server qualified as S

staticHandlers
  :: MonadIO f
  => LogAction f (WithSeverity Text) -> RuleSet -> S.Handlers f
staticHandlers logger rs =
  mconcat
    [ S.notificationHandler Msg.SMethod_Initialized $ \(_msg :: Msg.TNotificationMessage Msg.Method_Initialized) ->
        logger <& WithSeverity "Client initialized" Info
    , S.notificationHandler Msg.SMethod_WorkspaceDidChangeConfiguration $ \(_msg :: Msg.TNotificationMessage Msg.Method_WorkspaceDidChangeConfiguration) ->
        logger <& WithSeverity "Configuration changed" Info
    , S.notificationHandler Msg.SMethod_TextDocumentDidOpen (handleDidOpen logger)
    , S.notificationHandler Msg.SMethod_TextDocumentDidClose (handleDidClose logger)
    , S.notificationHandler Msg.SMethod_TextDocumentDidChange (handleDidChange logger)
    ]
    <> Formatting.handlers logger rs

runServer :: RuleSet -> IO Int
runServer rs =
  S.runServer $
    S.ServerDefinition
      { configSection = "jbeam-lsp"
      , parseConfig = \_ _ -> Right Formatting.Config
      , onConfigChange = const $ pure ()
      , defaultConfig = Formatting.Config
      , doInitialize = \env _ -> pure (Right env)
      , staticHandlers = const $ staticHandlers defaultClientLogger rs
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

handleDidOpen
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . (MonadIO m2, Msg.MessageParams m1 ~ J.DidOpenTextDocumentParams)
  => LogAction m2 (WithSeverity Text)
  -> Msg.TNotificationMessage m1
  -> m2 ()
handleDidOpen logger (Msg.TNotificationMessage _ _ (J.DidOpenTextDocumentParams textDoc)) = do
  let J.TextDocumentItem {J._uri = uri, J._text = txt} = textDoc
  liftIO $ Docs.update uri txt
  logger <& WithSeverity ("Document opened: " <> J.getUri uri) Info

handleDidChange
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . ( MonadIO m2
     , Msg.MessageParams m1 ~ J.DidChangeTextDocumentParams
     )
  => LogAction m2 (WithSeverity Text)
  -> Msg.TNotificationMessage m1
  -> m2 ()
handleDidChange logger (Msg.TNotificationMessage _ _ (J.DidChangeTextDocumentParams docId changes)) = do
  let J.VersionedTextDocumentIdentifier {_uri = uri} = docId
  forM_ changes $ \(J.TextDocumentContentChangeEvent change) ->
    case change of
      J.InL (J.TextDocumentContentChangePartial {J._text = txt}) ->
        liftIO $ Docs.update uri txt
      J.InR (J.TextDocumentContentChangeWholeDocument txt) ->
        liftIO $ Docs.update uri txt
  logger <& WithSeverity ("Document changed: " <> J.getUri uri) Info

handleDidClose
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . ( MonadIO m2
     , Msg.MessageParams m1 ~ J.DidCloseTextDocumentParams
     )
  => LogAction m2 (WithSeverity Text)
  -> Msg.TNotificationMessage m1
  -> m2 ()
handleDidClose logger (Msg.TNotificationMessage _ _ (J.DidCloseTextDocumentParams docId)) = do
  let J.TextDocumentIdentifier {_uri = uri} = docId
  liftIO $ Docs.delete uri
  logger <& WithSeverity ("Document closed: " <> J.getUri uri) Info
