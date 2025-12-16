{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JbeamEdit.LSP.Handlers.Formatting (handlers) where

import Colog.Core (
  LogAction (..),
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
import JbeamEdit.LSP.Logging
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
  Uri (..),
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
    formattingHandler
      :: Msg.TRequestMessage Msg.Method_TextDocumentFormatting
      -> ( Either
             (Msg.TResponseError Msg.Method_TextDocumentFormatting)
             (Msg.MessageResult Msg.Method_TextDocumentFormatting)
           -> S.LspM config ()
         )
      -> S.LspM config ()
    formattingHandler req responder = do
      logDebug logAction "formattingHandler invoked"
      let Msg.TRequestMessage _ _ _ (params :: J.DocumentFormattingParams) = req
      handleParams rs logAction params responder

handleParams
  :: RuleSet
  -> LogAction IO (WithSeverity String)
  -> J.DocumentFormattingParams
  -> ( Either
         (Msg.TResponseError Msg.Method_TextDocumentFormatting)
         (Msg.MessageResult Msg.Method_TextDocumentFormatting)
       -> S.LspM config ()
     )
  -> S.LspM config ()
handleParams rs logAction params responder = do
  let J.DocumentFormattingParams {J._textDocument = textDocId} = params
      J.TextDocumentIdentifier {J._uri = uri} = textDocId
      sendNoUpdate = responder (Right (J.InR J.Null))

  mText <- liftIO $ Docs.get uri
  case mText of
    Nothing -> do
      logDebug logAction ("no document in store for " <> J.getUri uri)
      sendNoUpdate
    Just txt ->
      case JbeamP.parseNodes . LBS.fromStrict . encodeUtf8 $ txt of
        Left err -> do
          logError logAction ("Parse error: " <> err)
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
