module Parsing.Common.Helpers (
  Parser,
  byteChar,
  charNotEqWord8,
  failingParser,
  toChar,
  toWord8,
  parseWord8s,
  tryParsers,
  skipWhiteSpace,
  parseBool,
) where

import Data.Char (isSpace)

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE (fromList)
import Data.Set qualified as S
import Data.Text qualified as T
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B

type Parser = MP.Parsec Void ByteString

---
--- helpers
---
toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

toChar :: Word8 -> Char
toChar = chr . fromIntegral

charNotEqWord8 :: Char -> Word8 -> Bool
charNotEqWord8 c w = toWord8 c /= w

tryParsers :: [Parser a] -> Parser a
tryParsers = asum . map MP.try

byteChar :: Char -> Parser Word8
byteChar = B.char . toWord8

skipWhiteSpace :: Parser ()
skipWhiteSpace = B.space

parseWord8s :: (T.Text -> a) -> Parser [Word8] -> Parser a
parseWord8s f bsParser = do
  bs' <- bsParser
  case decodeUtf8' (BS.pack bs') of
    Right text' -> pure (f text')
    Left _ -> empty

failingParser :: [String] -> Parser a
failingParser expLabels = unexpTok >>= flip MP.failure expToks
  where
    unexpTok =
      Just . MP.Tokens . NE.fromList . BS.unpack
        <$> MP.takeWhile1P Nothing isNotFinalChar
    expToks = S.fromList . map (MP.Label . NE.fromList) $ expLabels
    isNotFinalChar w =
      let c = toChar w
       in not (isSpace c) && notElem c [',', ']', '}']

parseBool :: Parser Bool
parseBool = MP.choice [B.string "true", B.string "false"] <&> (== "true")
