module Parsing.Common.Helpers (
  Parser,
  byteChar,
  charNotEqWord8,
  failingParser,
  toChar,
  toWord8,
  wordIsSpace,
  parseWord8s,
  tryParsers,
  skipWhiteSpace,
  parseBool,
) where

import Control.Applicative (asum, empty)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr, isSpace, ord)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE (fromList)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B

type Parser m a = MP.ParsecT Void ByteString m a

---
--- helpers
---
toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

toChar :: Word8 -> Char
toChar = chr . fromIntegral

wordIsSpace :: Word8 -> Bool
wordIsSpace = isSpace . toChar

charNotEqWord8 :: Char -> Word8 -> Bool
charNotEqWord8 c w = toWord8 c /= w

tryParsers :: [Parser m a] -> Parser m a
tryParsers = asum . map MP.try

byteChar :: Char -> Parser m Word8
byteChar = B.char . toWord8

skipWhiteSpace :: Parser m ()
skipWhiteSpace = B.space

parseWord8s :: (T.Text -> a) -> Parser m [Word8] -> Parser m a
parseWord8s f bsParser = do
  bs' <- bsParser
  case decodeUtf8' (BS.pack bs') of
    Right text' -> pure (f text')
    Left _ -> empty

failingParser :: [String] -> Parser m a
failingParser expLabels = unexpTok >>= flip MP.failure expToks
  where
    unexpTok =
      Just . MP.Tokens . NE.fromList . BS.unpack
        <$> MP.takeWhile1P Nothing isNotFinalChar
    expToks = S.fromList . map (MP.Label . NE.fromList) $ expLabels
    isNotFinalChar w =
      let c = toChar w
       in not (isSpace c) && notElem c [',', ']', '}']

parseBool :: Parser m Bool
parseBool = MP.choice [B.string "true", B.string "false"] <&> (== "true")
