module Parsing.Internal
  ( topNodeParser
  , charNotEqWord8
  , toChar
  , toWord8
  , nodeParser
  ) where

import Control.Applicative (Alternative(..), (<|>), asum, optional)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr, isSpace, ord)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty qualified as LV (fromList)
import Data.Set qualified as S
import Data.Text.Encoding (decodeUtf8')
import Data.Vector qualified as V (fromList)
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (scientific, signed)
import Text.Megaparsec.Char qualified as C

import Data.Text qualified as T
import Data.Word (Word8)

import Node (Node(..))

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

separatorParser :: Parser ()
separatorParser =
  tryParsers [commaSeparator, whitespaceSeparator] *> skipWhiteSpace
  where
    commaSeparator = skipWhiteSpace *> byteChar ','
    whitespaceSeparator = B.spaceChar

parseWord8s :: (T.Text -> a) -> Parser [Word8] -> Parser a
parseWord8s f bsParser = do
  bs' <- bsParser
  case decodeUtf8' (BS.pack bs') of
    Right text' -> pure (f text')
    Left _ -> empty

---
--- selectors for numbers, comments, strings and bools
---
numberParser :: Parser Node
numberParser = fmap Number signedScientific
  where
    scientific = L.scientific
    signedScientific = L.signed B.space scientific

multilineCommentParser :: Parser Node
multilineCommentParser =
  C.string "/*" >> parseComment (MP.manyTill B.asciiChar (B.string "*/"))
  where
    parseComment = parseWord8s (MultilineComment . T.strip)

singlelineCommentParser :: Parser Node
singlelineCommentParser =
  C.string "//" *> parseComment (MP.some (MP.satisfy (charNotEqWord8 '\n')))
  where
    parseComment = parseWord8s (SinglelineComment . T.strip)

commentParser :: Parser Node
commentParser =
  tryParsers [multilineCommentParser, singlelineCommentParser] <?> "comment"

nullParser :: Parser Node
nullParser = C.string "null" $> Null

boolParser :: Parser Node
boolParser =
  MP.choice [C.string "true", C.string "false"] <&> Bool . (== "true")

stringParser :: Parser Node
stringParser = parseWord8s String string
  where
    validString =
      byteChar '"' *> MP.some (MP.satisfy (charNotEqWord8 '"')) <* byteChar '"'
    emptyString = C.string "\"\"" >> pure []
    string = emptyString <|> validString

failingScalarParser :: Parser Node
failingScalarParser = unexpTok >>= flip MP.failure expToks
  where
    unexpTok =
      Just . MP.Tokens . LV.fromList . BS.unpack
        <$> MP.takeWhile1P Nothing isNotFinalChar
    expToks =
      S.fromList . map (MP.Label . LV.fromList)
        $ ["a valid scalar", "object", "array"]
    isNotFinalChar w =
      let c = toChar w
       in not (isSpace c) && notElem c [',', ']', '}']

scalarParser :: Parser Node
scalarParser =
  tryScalarParsers
    [stringParser, commentParser, numberParser, boolParser, nullParser]
  where
    tryScalarParsers = MP.try . tryParsers . map MP.hidden

nodeParser :: Parser Node
nodeParser = skipWhiteSpace *> (anyNode <|> failingScalarParser)
  where
    anyNode = MP.try (tryParsers [arrayParser, objectParser, scalarParser])

---
--- selectors for objects, object keys and arrays
---
arrayParser :: Parser Node
arrayParser = do
  _ <- byteChar '['
  elems <- MP.sepEndBy nodeParser separatorParser
  _ <- optional separatorParser
  _ <- byteChar ']'
  pure . Array . V.fromList $ elems

objectKeyParser :: Parser Node
objectKeyParser = do
  _ <- skipWhiteSpace
  key <- MP.try (stringParser <?> "string")
  _ <- skipWhiteSpace
  _ <- byteChar ':'
  value <- nodeParser
  let obj = ObjectKey (key, value)
  c <- MP.lookAhead B.asciiChar
  case toChar c of
    '}' -> pure obj
    _ -> separatorParser $> obj

objectParser :: Parser Node
objectParser = do
  _ <- byteChar '{'
  keys <- MP.some (commentParser <|> objectKeyParser)
  _ <- optional separatorParser
  _ <- byteChar '}'
  pure . Object . V.fromList $ keys

topNodeParser :: Parser Node
topNodeParser = nodeParser <* skipWhiteSpace <* MP.eof
