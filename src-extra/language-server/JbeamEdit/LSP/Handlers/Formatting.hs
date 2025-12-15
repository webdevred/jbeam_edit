{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JbeamEdit.LSP.Handlers.Formatting (handlers) where

import Colog.Core (
  LogAction (..),
  Severity (..),
  WithSeverity (..),
 )
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Formatting qualified as Fmt
import JbeamEdit.Formatting.Rules (RuleSet)
import JbeamEdit.LSP.Services.DocumentStore qualified as Docs
import JbeamEdit.Parsing.Jbeam qualified as JbeamP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types qualified as J (
  DocumentFormattingParams (..),
  Null (..),
  Position (..),
  Range (..),
  TextDocumentIdentifier (..),
  TextEdit (..),
  type (|?) (..),
 )
import Language.LSP.Server qualified as S

handlers
  :: RuleSet
  -> LogAction IO (WithSeverity String)
  -> S.Handlers (S.LspM config)
handlers rs logAction =
  S.requestHandler Msg.SMethod_TextDocumentFormatting formattingHandler
  where
    logMsg :: MonadIO m => Severity -> String -> m ()
    logMsg sev msg =
      liftIO $ unLogAction logAction (WithSeverity msg sev)

    formattingHandler
      :: Msg.TRequestMessage Msg.Method_TextDocumentFormatting
      -> ( Either
             (Msg.TResponseError Msg.Method_TextDocumentFormatting)
             (Msg.MessageResult Msg.Method_TextDocumentFormatting)
           -> S.LspM config ()
         )
      -> S.LspM config ()
    formattingHandler req responder = do
      logMsg Debug "formattingHandler invoked"
      let Msg.TRequestMessage _ _ _ (params :: J.DocumentFormattingParams) = req
      handleParams rs logMsg params responder

handleParams
  :: RuleSet
  -> (Severity -> String -> S.LspM config ())
  -> J.DocumentFormattingParams
  -> ( Either
         (Msg.TResponseError Msg.Method_TextDocumentFormatting)
         (Msg.MessageResult Msg.Method_TextDocumentFormatting)
       -> S.LspM config ()
     )
  -> S.LspM config ()
handleParams rs logMsg params responder = do
  let J.DocumentFormattingParams {J._textDocument = textDocId} = params
      J.TextDocumentIdentifier {J._uri = uri} = textDocId
      sendNoUpdate = responder (Right (J.InR J.Null))

  mText <- liftIO $ Docs.get uri
  case mText of
    Nothing -> do
      logMsg Debug ("no document in store for " <> show uri)
      sendNoUpdate
    Just txt ->
      case JbeamP.parseNodes . LBS.fromStrict . encodeUtf8 $ txt of
        Left err -> do
          logMsg Error ("Parse error: " <> show err)
          sendNoUpdate
        Right node ->
          case runFormatNode rs txt node of
            Nothing ->
              sendNoUpdate
            Just edit ->
              responder (Right (J.InL [edit]))

runFormatNode :: RuleSet -> T.Text -> Node -> Maybe J.TextEdit
runFormatNode ruleSet txt node =
  let newText = Fmt.formatNode ruleSet node
      edit =
        J.TextEdit
          { J._range = wholeRange txt
          , J._newText = newText
          }
   in if newText /= txt
        then Just edit
        else Nothing

wholeRange :: T.Text -> J.Range
wholeRange txt =
  let ls = T.lines txt
      numLines = max 1 (length ls)
      lastLineLen =
        case reverse ls of
          [] -> 0
          (lastLine : _) -> T.length lastLine
   in J.Range
        (J.Position 0 0)
        ( J.Position
            (fromIntegral numLines)
            (fromIntegral lastLineLen)
        )
