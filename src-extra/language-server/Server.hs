{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer) where

import Formatting.Rules (RuleSet)
import Handlers.Formatting qualified as Formatting
import IOUtils
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
import Services.DocumentStore qualified as Docs

staticHandlers :: RuleSet -> S.Handlers (S.LspM config)
staticHandlers rs =
  mconcat
    [ S.notificationHandler Msg.SMethod_Initialized $ \_notif ->
        liftIO $ putErrorLine "Client initialized"
    , S.notificationHandler Msg.SMethod_WorkspaceDidChangeConfiguration $ \_notif ->
        liftIO $ putErrorLine "Configuration changed"
    , S.notificationHandler Msg.SMethod_TextDocumentDidOpen handleDidOpen
    , S.notificationHandler Msg.SMethod_TextDocumentDidClose handleDidClose
    , S.notificationHandler Msg.SMethod_TextDocumentDidChange handleDidChange
    ]
    <> Formatting.handlers rs

-- | Starta LSP-servern
runServer :: RuleSet -> IO Int
runServer rs =
  S.runServer $
    S.ServerDefinition
      { configSection = "jbeam-lsp"
      , parseConfig = \_ _ -> Right ()
      , onConfigChange = const >> pure $ pass
      , defaultConfig = ()
      , doInitialize = \env _req -> pure (Right env)
      , staticHandlers = const $ staticHandlers rs
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
   . (MonadIO m2, Msg.MessageParams m1 ~ J.DidOpenTextDocumentParams)
  => Msg.TNotificationMessage m1 -> m2 ()
handleDidOpen (Msg.TNotificationMessage _ _ (J.DidOpenTextDocumentParams textDoc)) =
  let J.TextDocumentItem {J._uri = uri, J._text = txt} = textDoc
   in liftIO $ Docs.open uri txt

-- | didChange: update document in DocumentStore
handleDidChange
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . ( MonadIO m2
     , Msg.MessageParams m1 ~ J.DidChangeTextDocumentParams
     )
  => Msg.TNotificationMessage m1 -> m2 ()
handleDidChange (Msg.TNotificationMessage _ _ (J.DidChangeTextDocumentParams docId changes)) =
  let J.VersionedTextDocumentIdentifier {_uri = uri} = docId
   in case changes of
        (J.TextDocumentContentChangeEvent change : _) ->
          case change of
            J.InL (J.TextDocumentContentChangePartial {J._text = txt}) -> liftIO $ Docs.update uri txt
            J.InR (J.TextDocumentContentChangeWholeDocument txt) -> liftIO $ Docs.update uri txt
        _ -> pass

handleDidClose
  :: forall
    {f :: Msg.MessageDirection}
    {m1 :: Msg.Method f Msg.Notification}
    {m2 :: Type -> Type}
   . ( MonadIO m2
     , Msg.MessageParams m1 ~ J.DidCloseTextDocumentParams
     )
  => Msg.TNotificationMessage m1 -> m2 ()
handleDidClose (Msg.TNotificationMessage _ _ (J.DidCloseTextDocumentParams docId)) =
  let J.TextDocumentIdentifier {_uri = uri} = docId in liftIO (Docs.delete uri)
