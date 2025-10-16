{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Parsing.DSL (
  JbflParser,
  parseDSL,
  patternSelectorParser,
  keyPropertyPairParser,
  ruleSetParser,
) where

import Core.NodePath
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.List.NonEmpty qualified as NE (fromList)
import Data.Map qualified as M (fromList, fromListWith, union)
import Data.Sequence qualified as Seq (fromList)
import Data.Set qualified as S (fromList)
import Formatting.Rules
import Parsing.Common
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (
  decimal,
  skipBlockComment,
  skipLineComment,
 )

type JbflParser a = Parser Identity a

objectKeyParser :: JbflParser NodePatternSelector
objectKeyParser = byteChar '.' *> key
  where
    key = parseWord8s (Selector . ObjectKey) (MP.some . MP.satisfy $ p)
    p w =
      let c = toChar w
       in not (isSpace c) && c `notElem` ['[', '.']

objectIndexParser :: JbflParser NodePatternSelector
objectIndexParser = byteChar '.' *> index
  where
    index = Selector . ObjectIndex <$> L.decimal

arrayIndexParser :: JbflParser NodePatternSelector
arrayIndexParser = byteChar '[' *> index <* byteChar ']'
  where
    index = Selector . ArrayIndex <$> L.decimal

patternSelectorParser :: JbflParser NodePatternSelector
patternSelectorParser =
  tryParsers
    [ B.string ".*" $> AnyObjectKey
    , B.string "[*]" $> AnyArrayIndex
    , objectIndexParser <?> "object index"
    , objectKeyParser <?> "object key"
    , arrayIndexParser <?> "array index"
    ]

patternParser :: JbflParser NodePattern
patternParser = skipWhiteSpace *> patternSelectors <* skipWhiteSpace
  where
    patternSelectors = NodePattern . Seq.fromList <$> MP.some patternSelectorParser

tryDecodeKey :: [Word8] -> (Text -> Maybe SomeKey) -> Maybe SomeKey
tryDecodeKey bs f =
  case decodeUtf8' (BS.pack bs) of
    Right text' -> f text'
    Left _ -> Nothing

propertyParser :: SomeKey -> JbflParser (SomeKey, SomeProperty)
propertyParser (SomeKey key) = do
  _ <- byteChar ':'
  skipWhiteSpace
  val <- parseValueForKey key
  separatorParser
  skipComment
  let prop = SomeProperty key
   in pure (SomeKey key, prop val)

parseValueForKey :: PropertyKey a -> JbflParser a
parseValueForKey NoComplexNewLine = parseBool <?> "bool"
parseValueForKey PadAmount = L.decimal <?> "integer"
parseValueForKey PadDecimals = L.decimal <?> "integer"
parseValueForKey Indent = L.decimal <?> "integer"

skipComment :: JbflParser ()
skipComment = void . MP.many $ comment <* skipWhiteSpace
  where
    comment = tryParsers [L.skipLineComment "//", L.skipBlockComment "/*" "*/"]

keyPropertyPairParser :: JbflParser (SomeKey, SomeProperty)
keyPropertyPairParser = do
  skipComment
  offset <- MP.getOffset
  key <-
    MP.label "property name" $
      MP.some (MP.satisfy (\c -> toChar c `notElem` [' ', ':']))
  skipWhiteSpace
  let unexpTok = pure . Just . MP.Tokens . NE.fromList $ key
      expToks =
        S.fromList . map (MP.Label . NE.fromList . toString . keyName) $
          allProperties
      failParser u = MP.setOffset offset *> MP.failure u expToks
      key' = tryDecodeKey key (`lookupKey` allProperties)
   in maybe (unexpTok >>= failParser) propertyParser key'

separatorParser :: JbflParser ()
separatorParser = skipWhiteSpace *> void (byteChar ';') <* skipWhiteSpace

ruleParser :: JbflParser (NodePattern, Map SomeKey SomeProperty)
ruleParser = do
  pat <- patternParser
  skipWhiteSpace
  _ <- byteChar '{'
  skipWhiteSpace
  props <- MP.some (MP.try keyPropertyPairParser)
  _ <- byteChar '}'
  skipWhiteSpace
  pure (pat, M.fromList props)

ruleSetParser :: JbflParser RuleSet
ruleSetParser = RuleSet . M.fromListWith M.union <$> MP.some singleRuleSet
  where
    singleRuleSet = skipComment *> ruleParser <* skipComment

parseDSL :: ByteString -> Either Text RuleSet
parseDSL input
  | BS.null input = pure newRuleSet
  | otherwise =
      first formatErrors . MP.parse (ruleSetParser <* MP.eof) "<input>" $ input
