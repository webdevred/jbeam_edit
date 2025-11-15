{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module JbeamEdit.Parsing.DSL (
  JbflParser,
  parseDSL,
  patternSelectorParser,
  keyPropertyPairParser,
  ruleSetParser,
) where

import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BS
import Data.Char (isSpace)
import Data.Functor (void, ($>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty qualified as NE (fromList)
import Data.Map (Map)
import Data.Map qualified as M (fromList, fromListWith, union)
import Data.Sequence qualified as Seq (fromList)
import Data.Set qualified as S (fromList)
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import JbeamEdit.Core.NodePath
import JbeamEdit.Formatting.Rules
import JbeamEdit.Parsing.Common
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
       in not (isSpace c) && c `notElem` [',', '[', '.']

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
patternParser = do
  pat <- skipWhiteSpace *> patternSelectors
  c <- MP.lookAhead B.asciiChar
  case toChar c of
    ',' -> byteChar ',' $> pat
    _ -> skipWhiteSpace $> pat
  where
    patternSelectors = NodePattern . Seq.fromList <$> MP.some patternSelectorParser

tryDecodeKey :: [Word8] -> (Text -> Maybe SomeKey) -> Maybe SomeKey
tryDecodeKey bs f =
  case decodeUtf8' (BS.toStrict $ BS.pack bs) of
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
parseValueForKey ForceComplexNewLine = parseBool <?> "bool"
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
        S.fromList . map (MP.Label . NE.fromList . T.unpack . keyName) $
          allProperties
      failParser u = MP.setOffset offset *> MP.failure u expToks
      key' = tryDecodeKey key (`lookupKey` allProperties)
   in maybe (unexpTok >>= failParser) propertyParser key'

separatorParser :: JbflParser ()
separatorParser = skipWhiteSpace *> void (byteChar ';') <* skipWhiteSpace

ruleParser :: JbflParser ([NodePattern], Map SomeKey SomeProperty)
ruleParser = do
  pats <- MP.some patternParser
  skipWhiteSpace
  _ <- byteChar '{'
  skipWhiteSpace
  props <- MP.some (MP.try keyPropertyPairParser)
  _ <- byteChar '}'
  skipWhiteSpace
  pure (pats, M.fromList props)

separateRulesets :: [([a], b)] -> [(a, b)]
separateRulesets rs = [(pat, props) | (pats, props) <- rs, pat <- pats]

ruleSetParser :: JbflParser RuleSet
ruleSetParser = RuleSet . M.fromListWith M.union . separateRulesets <$> MP.some singleRuleSet
  where
    singleRuleSet = skipComment *> ruleParser <* skipComment

parseDSL :: BS.ByteString -> Either Text RuleSet
parseDSL input
  | BS.null input = pure newRuleSet
  | otherwise =
      first formatErrors . MP.parse (ruleSetParser <* MP.eof) "<input>" $
        input
