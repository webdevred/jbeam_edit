{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JbeamEdit.LSP.Handlers.Formatting (handlers) where

import Core.Node (Node)
import Data.Text qualified as T
import Formatting qualified as Fmt
import Formatting.Rules (RuleSet)
import IOUtils
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
import Parsing.Jbeam qualified as JbeamP
import JbeamEdit.LSP.Services.DocumentStore qualified as Docs

putErrorLine' :: MonadIO m => Text -> m ()
putErrorLine' = liftIO . putErrorLine

handlers :: RuleSet -> S.Handlers (S.LspM config)
handlers rs =
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
      putErrorLine' "DEBUG: formattingHandler invoked"
      let Msg.TRequestMessage _ _ _ (params :: J.DocumentFormattingParams) = req
      handleParams rs params responder

handleParams
  :: RuleSet
  -> J.DocumentFormattingParams
  -> ( Either
         (Msg.TResponseError Msg.Method_TextDocumentFormatting)
         (Msg.MessageResult Msg.Method_TextDocumentFormatting)
       -> S.LspM config ()
     )
  -> S.LspM config ()
handleParams rs params responder = do
  let J.DocumentFormattingParams {J._textDocument = textDocId} = params
      J.TextDocumentIdentifier {J._uri = uri} = textDocId
      sendNoUpdate = responder (Right (J.InR J.Null))
  mText <- liftIO $ Docs.get uri
  case mText of
    Nothing ->
      putErrorLine' ("DEBUG: no document in store for " <> show uri)
        >> sendNoUpdate
    Just txt ->
      case JbeamP.parseNodes (encodeUtf8 txt) of
        Left err ->
          putErrorLine' ("Parse error: " <> show err) >> sendNoUpdate
        Right node ->
          case runFormatNode rs txt node of
            Nothing -> responder (Right (J.InR J.Null))
            Just edit -> responder (Right (J.InL [edit]))

runFormatNode :: RuleSet -> T.Text -> Node -> Maybe J.TextEdit
runFormatNode ruleSet txt node =
  let newText = Fmt.formatNode ruleSet node
      edit = J.TextEdit {J._range = wholeRange txt, J._newText = newText}
   in bool Nothing (Just edit) (newText /= txt)

wholeRange :: Text -> J.Range
wholeRange txt =
  let ls = lines txt
      numLines = max 1 (length ls)
      lastLineLen =
        case reverse ls of
          [] -> 0
          (lastLine : _) -> T.length lastLine
   in J.Range
        (J.Position 0 0)
        (J.Position (fromIntegral numLines) (fromIntegral lastLineLen))
