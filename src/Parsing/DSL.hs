{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Parsing.DSL (
  parseDSL,
) where

import Core.NodePath
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Functor (void, ($>))
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import Formatting.Rules
import Parsing.Common
import Text.Megaparsec ((<?>))

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as LV (fromList)
import Data.Map qualified as M (fromList, fromListWith, union)
import Data.Sequence qualified as Seq (fromList)
import Data.Set qualified as S (fromList)
import Data.Text qualified as T
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (decimal)

keyParser :: Parser NodePatternSelector
keyParser = byteChar '.' *> key
  where
    key = parseWord8s (Selector . ObjectKey) (MP.some . MP.satisfy $ p)
    p w =
      let c = toChar w
       in not (isSpace c) && c `notElem` ['[', '.']

indexParser :: Parser NodePatternSelector
indexParser = byteChar '[' *> index <* byteChar ']'
  where
    index = Selector . ArrayIndex <$> L.decimal

patternSelectorParser :: Parser NodePatternSelector
patternSelectorParser = skipWhiteSpace *> anySel <* skipWhiteSpace
  where
    anySel =
      tryParsers
        [ B.string ".*" $> AnyKey
        , B.string "[*]" $> AnyIndex
        , keyParser <?> "key"
        , indexParser <?> "index"
        ]

patternParser :: Parser NodePattern
patternParser = NodePattern . Seq.fromList <$> MP.some patternSelectorParser

tryDecodeKey :: [Word8] -> (Text -> Maybe SomeKey) -> Maybe SomeKey
tryDecodeKey bs f = do
  case decodeUtf8' (BS.pack bs) of
    Right text' -> f text'
    Left _ -> Nothing

propertyParser :: SomeKey -> Parser (SomeKey, SomeProperty)
propertyParser (SomeKey key) = do
  _ <- byteChar ':'
  skipWhiteSpace
  val <- parseValueForKey key
  separatorParser
  let prop = SomeProperty key
   in pure (SomeKey key, prop val)

parseValueForKey :: PropertyKey a -> Parser a
parseValueForKey NoComplexNewLine = parseBool <?> "bool"
parseValueForKey PadZeros = parseBool <?> "bool"
parseValueForKey PadAmount = L.decimal <?> "integer"

keyPropertyPairParser :: Parser (SomeKey, SomeProperty)
keyPropertyPairParser = do
  skipWhiteSpace
  offset <- MP.getOffset
  key <-
    MP.label "property name" $
      MP.some (MP.satisfy (\c -> toChar c `notElem` [' ', ':']))
  skipWhiteSpace
  let unexpTok = pure . Just . MP.Tokens . LV.fromList $ key
      expToks =
        S.fromList . map (MP.Label . LV.fromList . T.unpack . keyName) $
          allProperties
      failParser u = MP.setOffset offset *> MP.failure u expToks
      key' = tryDecodeKey key (`lookupKey` allProperties)
   in maybe (unexpTok >>= failParser) propertyParser key'

separatorParser :: Parser ()
separatorParser = skipWhiteSpace *> void (byteChar ';') <* skipWhiteSpace

ruleParser :: Parser (NodePattern, Map SomeKey SomeProperty)
ruleParser = do
  pat <- patternParser
  skipWhiteSpace
  _ <- byteChar '{'
  skipWhiteSpace
  props <- MP.some (MP.try keyPropertyPairParser)
  _ <- byteChar '}'
  pure (pat, M.fromList props)

ruleSetParser :: Parser RuleSet
ruleSetParser = RuleSet . M.fromListWith M.union <$> MP.some singleRuleSet
  where
    singleRuleSet = skipWhiteSpace *> ruleParser <* skipWhiteSpace

parseDSL :: ByteString -> Either Text RuleSet
parseDSL input
  | BS.null input = pure newRuleSet
  | otherwise =
      first formatErrors . MP.parse (ruleSetParser <* MP.eof) "<input>" $ input
