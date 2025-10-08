{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer) where

import Formatting.Rules (RuleSet)

import Formatting.Config qualified as FmtCfg
import Handlers.Formatting qualified as Formatting
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types qualified as J (
  DidChangeTextDocumentParams (..),
  DidOpenTextDocumentParams (..),
  TextDocumentContentChangeEvent (..),
  TextDocumentContentChangePartial (..),
  TextDocumentContentChangeWholeDocument (..),
  TextDocumentItem (..),
  VersionedTextDocumentIdentifier (..),
  type (|?) (..),
 )
import Language.LSP.Server qualified as S
import Services.DocumentStore qualified as Docs

staticHandlers :: RuleSet -> S.Handlers (S.LspM config)
staticHandlers rs =
  mconcat
    [ S.notificationHandler Msg.SMethod_Initialized $ \_notif ->
        liftIO $ putStrLn "Client initialized"
    , S.notificationHandler Msg.SMethod_WorkspaceDidChangeConfiguration $ \_notif ->
        liftIO $ putStrLn "Configuration changed"
    , S.notificationHandler Msg.SMethod_TextDocumentDidOpen handleDidOpen
    , S.notificationHandler Msg.SMethod_TextDocumentDidChange handleDidChange
    ]
    <> Formatting.handlers rs

-- | Starta LSP-servern
runServer :: IO Int
runServer = do
  ruleSet <- FmtCfg.readFormattingConfig
  S.runServer $
    S.ServerDefinition
      { configSection = "jbeam-lsp"
      , parseConfig = \_ _ -> Right ()
      , onConfigChange = const >> pure $ pass
      , defaultConfig = ()
      , doInitialize = \env _req -> pure (Right env)
      , staticHandlers = const $ staticHandlers ruleSet
      , interpretHandler = \env -> S.Iso (S.runLspT env) liftIO
      , options = S.defaultOptions
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
