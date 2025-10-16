{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Formatting (handlers) where

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
import Services.DocumentStore qualified as Docs

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
  mText <- liftIO $ Docs.get uri
  case mText of
    Nothing -> do
      putErrorLine' ("DEBUG: no document in store for " <> show uri)
      responder (Right (J.InR J.Null))
    Just txt ->
      case JbeamP.parseNodes (encodeUtf8 txt) of
        Left err -> do
          liftIO . putErrorLine' $ "Parse error: " <> show err
          responder (Right (J.InR J.Null))
        Right node -> runFormatNode responder rs txt node

runFormatNode
  :: (Either a ([J.TextEdit] J.|? J.Null) -> t)
  -> RuleSet
  -> Text
  -> Node
  -> t
runFormatNode responder ruleSet txt node =
  let newText = Fmt.formatNode ruleSet node
      edit = J.TextEdit {J._range = wholeRange txt, J._newText = newText}
   in if newText == txt
        then
          responder (Right (J.InR J.Null))
        else
          responder (Right (J.InL [edit]))

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
