module Parsing.Internal
  ( topNodeParser
  , charNotEqWord8
  , toChar
  , toWord8
  , stringParser
  , numberParser
  , boolParser
  , singlelineCommentParser
  , multilineCommentParser
  , arrayParser
  , objectParser
  ) where

import Control.Applicative (Alternative(..), (<|>), asum, optional)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr, isSpace, ord)
import Data.Functor (($>), (<&>))
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V (fromList)
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (lexeme, scientific, signed)
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

---
--- selectors for numbers, comments, strings and bools
---
numberParser :: Parser Node
numberParser = fmap Number signedScientific
  where
    spaceConsumer = B.space
    lexeme = L.lexeme spaceConsumer
    scientific = lexeme L.scientific
    signedScientific = L.signed spaceConsumer scientific

multilineCommentParser :: Parser Node
multilineCommentParser =
  C.string "/*" *> MP.manyTill B.asciiChar (B.string "*/") <&> parseComment
  where
    parseComment = MultilineComment . T.strip . decodeUtf8 . BS.pack

singlelineCommentParser :: Parser Node
singlelineCommentParser =
  C.string "//" *> MP.some (MP.satisfy (charNotEqWord8 '\n')) <&> parseComment
  where
    parseComment = SinglelineComment . T.strip . decodeUtf8 . BS.pack

commentParser :: Parser Node
commentParser =
  tryParsers [multilineCommentParser, singlelineCommentParser] <?> "comment"

nullParser :: Parser Node
nullParser = C.string "null" $> Null

boolParser :: Parser Node
boolParser =
  MP.choice [C.string "true", C.string "false"] <&> Bool . (== "true")

stringParser :: Parser Node
stringParser = string <&> String . decodeUtf8 . BS.pack
  where
    validString =
      byteChar '"' *> MP.some (MP.satisfy (charNotEqWord8 '"')) <* byteChar '"'
    emptyString = C.string "\"\"" >> pure []
    string = emptyString <|> validString

failingScalarParser :: Parser Node
failingScalarParser =
  MP.label "invalid scalar" $ do
    start <- MP.getOffset
    _ <- MP.takeWhile1P Nothing isFinalChar
    MP.setOffset start
    empty
  where
    isFinalChar w =
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
    anyNode =
      MP.try
        (tryParsers [arrayParser, objectParser, scalarParser <?> "a scalar"])

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
