module Parsing
  ( parseNodes
  ) where

import Control.Applicative ((<|>), asum, optional)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (pack)
import Data.Char (chr, ord)
import Data.Functor (($>), (<&>))
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V (fromList)
import Data.Void (Void)
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
toChar :: Word8 -> Char
toChar = chr . fromIntegral

tryParsers :: [Parser a] -> Parser a
tryParsers = asum . map MP.try

byteChar :: Char -> Parser Word8
byteChar = B.char . fromIntegral . ord

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

singlelineCommentParser :: Parser Node
singlelineCommentParser =
  C.string "//" *> MP.some (MP.satisfy ((/=) '\n' . toChar)) <&> parseComment
  where
    parseComment = SinglelineComment . T.strip . decodeUtf8 . BS.pack

commentParser :: Parser Node
commentParser = singlelineCommentParser

nullParser :: Parser Node
nullParser = C.string "null" $> Null

boolParser :: Parser Node
boolParser =
  MP.choice [C.string "true", C.string "false"] <&> Bool . (== "true")

stringParser :: Parser Node
stringParser = string <&> String . decodeUtf8 . BS.pack
  where
    validString =
      byteChar '"' *> MP.some (MP.satisfy ((/=) '"' . toChar)) <* byteChar '"'
    emptyString = C.string "\"\"" >> pure []
    string = emptyString <|> validString

scalarParser :: Parser Node
scalarParser =
  tryParsers [stringParser, commentParser, numberParser, boolParser, nullParser]

nodeParser :: Parser Node
nodeParser = skipWhiteSpace *> anyNode
  where
    anyNode = tryParsers [scalarParser, arrayParser, objectParser]

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
  key <- stringParser
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

parseNodes :: ByteString -> Either (MP.ParseErrorBundle ByteString Void) Node
parseNodes = MP.parse (nodeParser <* skipWhiteSpace <* MP.eof) "<input>"
