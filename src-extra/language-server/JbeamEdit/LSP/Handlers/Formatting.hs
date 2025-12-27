{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module JbeamEdit.LSP.Handlers.Formatting (handlers, Config (..)) where

import Colog.Core
import Control.Monad.IO.Class
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
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
  Uri (..),
  type (|?) (..),
 )
import Language.LSP.Server qualified as S

newtype Config = Config ()
  deriving (A.FromJSON, A.ToJSON, Generic, Show)

handlers
  :: MonadIO f
  => LogAction f (WithSeverity Text) -> RuleSet -> S.Handlers f
handlers logger rs =
  S.requestHandler Msg.SMethod_TextDocumentFormatting formattingHandler
  where
    formattingHandler req responder = do
      logger <& WithSeverity "formattingHandler invoked" Debug
      let Msg.TRequestMessage _ _ _ (params :: J.DocumentFormattingParams) = req
      handleParams rs logger params responder

handleParams
  :: MonadIO m
  => RuleSet
  -> LogAction m (WithSeverity Text)
  -> J.DocumentFormattingParams
  -> (Either a ([J.TextEdit] J.|? J.Null) -> m b)
  -> m b
handleParams rs logger params responder = do
  let J.DocumentFormattingParams {J._textDocument = textDocId} = params
      J.TextDocumentIdentifier {J._uri = uri} = textDocId
      sendNoUpdate = responder (Right (J.InR J.Null))

  mText <- liftIO $ Docs.get uri
  case mText of
    Nothing ->
      logger <& WithSeverity ("no document in store for " <> J.getUri uri) Info
        >> sendNoUpdate
    Just txt ->
      case JbeamP.parseNodes . LBS.fromStrict . encodeUtf8 $ txt of
        Left err -> logger <& WithSeverity ("Parse error: " <> err) Info >> sendNoUpdate
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
