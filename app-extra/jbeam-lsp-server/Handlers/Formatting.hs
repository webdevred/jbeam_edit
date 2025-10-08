{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.Formatting (handlers) where

import Formatting.Rules (RuleSet)

import Data.Text qualified as T
import Formatting qualified as Fmt
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
      liftIO $ putStrLn "DEBUG: formattingHandler invoked"
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
      liftIO . putStrLn $ "DEBUG: no document in store for " ++ show uri
      responder (Right (J.InR J.Null))
    Just txt ->
      case JbeamP.parseNodes (encodeUtf8 txt) of
        Left err -> do
          liftIO . putStrLn $ "Parse error: " ++ show err
          responder (Right (J.InR J.Null))
        Right node -> do
          let newText = Fmt.formatNode rs node
              edit = J.TextEdit {J._range = wholeRange txt, J._newText = newText}
          responder (Right (J.InL [edit]))

wholeRange :: Text -> J.Range
wholeRange txt =
  let numLines = max 1 . length . lines $ txt
      lastLineLen = T.length $ T.takeWhileEnd (/= '\n') txt
   in J.Range
        (J.Position 0 0)
        (J.Position (fromIntegral (numLines - 1)) (fromIntegral lastLineLen))
