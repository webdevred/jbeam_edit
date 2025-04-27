module Parsing
  ( parseNodes
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as C
import Data.Attoparsec.ByteString.Char8 ((<?>))
import Data.Attoparsec.Internal.Types (Parser)
import Data.ByteString (ByteString)
import Data.Functor (($>), (<&>))
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V (fromList)

import Data.Text qualified as T

import Node (Node(..))

---
--- selector for white space and
--- for separator for array and object children
---
skipWhiteSpace :: C.Parser ()
skipWhiteSpace = C.skipSpace

separatorParser :: Parser ByteString ()
separatorParser = (commaSeparator <|> whitespaceSeparator) *> skipWhiteSpace
  where
    commaSeparator = skipWhiteSpace *> C.char ','
    whitespaceSeparator = C.space

---
--- selectors for numbers, comments, strings and bools
---
numberParser :: Parser ByteString Node
numberParser = fmap Number C.scientific

multilineCommentParser :: Parser ByteString Node
multilineCommentParser =
  C.string "/*" *> C.manyTill C.anyChar (C.string "*/") <&> parseComment
  where
    parseComment = MultilineComment . T.strip . T.pack

singlelineCommentParser :: Parser ByteString Node
singlelineCommentParser =
  C.string "//" *> C.takeWhile ('\n' /=) <&> parseComment
  where
    parseComment = SinglelineComment . T.strip . decodeUtf8

commentParser :: Parser ByteString Node
commentParser = multilineCommentParser <|> singlelineCommentParser

nullParser :: Parser ByteString Node
nullParser = C.string "null" $> Null

boolParser :: Parser ByteString Node
boolParser = C.choice [C.string "true", C.string "false"] <&> Bool . (== "true")

stringParser :: Parser ByteString Node
stringParser =
  (C.char '"' *> C.takeWhile ('"' /=) <* C.char '"') <&> String . decodeUtf8

scalarParser :: Parser ByteString Node
scalarParser =
  stringParser <|> commentParser <|> numberParser <|> boolParser <|> nullParser

nodeParser :: Parser ByteString Node
nodeParser = skipWhiteSpace *> anyNode
  where
    anyNode = scalarParser <|> arrayParser <|> objectParser

---
--- selectors for objects, object keys and arrays
---
arrayElemParser :: Parser ByteString Node
arrayElemParser = do
  n <- nodeParser
  c <- C.peekChar
  case c of
    Just ']' -> pure n
    _ -> separatorParser $> n

arrayParser :: Parser ByteString Node
arrayParser = do
  _ <- C.char '['
  elems <- C.many' arrayElemParser
  _ <- C.char ']'
  pure . Array . V.fromList $ elems

objectKeyParser :: Parser ByteString Node
objectKeyParser = do
  _ <- skipWhiteSpace
  key <- stringParser
  _ <- skipWhiteSpace
  _ <- C.char ':'
  value <- nodeParser
  let obj = ObjectKey (key, value)
  c <- C.peekChar
  case c of
    Just '}' -> pure obj
    _ -> separatorParser $> obj

objectParser :: Parser ByteString Node
objectParser = do
  _ <- C.char '{'
  keys <- C.many' (commentParser <|> objectKeyParser)
  _ <- C.char '}'
  pure . Object . V.fromList $ keys

parseNodes :: ByteString -> Either String Node
parseNodes =
  C.parseOnly
    $ nodeParser <* skipWhiteSpace <* (C.endOfInput <?> "unexpected input")
