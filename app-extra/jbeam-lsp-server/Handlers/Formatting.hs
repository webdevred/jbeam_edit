{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Formatting (handlers) where

import Language.LSP.Server (Handlers, LspM, requestHandler)

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
import Parsing.Jbeam qualified as JbeamP
import Services.DocumentStore qualified as Docs

handlers :: Handlers (LspM config)
handlers =
  mconcat
    [ requestHandler Msg.SMethod_TextDocumentFormatting $ \req responder ->
        -- req :: Msg.TRequestMessage ...
        -- responder :: Either (Msg.TResponseError ...) (Msg.MessageResult ...) -> LspM config ()
        case req of
          Msg.TRequestMessage _ _ _ params -> handleParams params responder
    ]

handleParams
  :: MonadIO m
  => J.DocumentFormattingParams
  -> (Either a ([J.TextEdit] J.|? J.Null) -> m b)
  -> m b
handleParams params responder =
  case params of
    -- pattern-matcha konkret för att få uri utan ambiguitet
    J.DocumentFormattingParams
      { J._textDocument = J.TextDocumentIdentifier {J._uri = uri}
      } -> do
        mText <- liftIO $ Docs.get uri

        case mText of
          Nothing ->
            -- returnera JSON null
            responder (Right $ J.InR J.Null)
          Just txt ->
            -- anta parseNodes :: ByteString -> Either Text Node
            case JbeamP.parseNodes (encodeUtf8 txt) of
              Left perr -> do
                -- logga parserfel; returnera null (eller publicera diagnostics om du vill)
                liftIO $ putStrLn ("Formatting parse error: " ++ toString perr)
                responder (Right $ J.InR J.Null)
              Right node -> do
                let newText = Fmt.formatNode Fmt.newRuleSet node
                    edit =
                      J.TextEdit
                        { J._range = wholeRange txt
                        , J._newText = newText
                        }
                responder (Right $ J.InL [edit])

wholeRange :: Text -> J.Range
wholeRange txt =
  let ls = lines txt
      numLines = max 1 (length ls)
      lastLineLen = maybe 0 (T.length . last) (nonEmpty ls)
   in J.Range
        (J.Position 0 0)
        ( J.Position
            (fromIntegral (numLines - 1))
            (fromIntegral lastLineLen)
        )
