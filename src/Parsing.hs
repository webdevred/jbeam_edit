module Parsing
  ( parseNodes
  , Node(..)
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as C
import Data.Attoparsec.ByteString.Char8 ((<?>))
import Data.Attoparsec.Internal.Types (Parser)
import Data.ByteString (ByteString)
import Data.Functor (($>), (<&>))
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V (fromList)

type Object = Vector Node

type ObjectKey = (Node, Node)

type Array = Vector Node

data Node
  = Array Array
  | Object Object
  | ObjectKey ObjectKey
  | String Text
  | Number Scientific
  | Bool Bool
  | SinglelineComment Text
  | MultilineComment Text
  | Null
  deriving (Show, Eq)

---
--- selector for white space and
--- for separator for array and object children
---
skipWhiteSpace :: C.Parser ()
skipWhiteSpace = C.skipSpace

separatorSelector :: Parser ByteString ()
separatorSelector = (commaSeparator <|> whitespaceSeparator) *> skipWhiteSpace
  where
    commaSeparator = skipWhiteSpace *> C.char ','
    whitespaceSeparator = C.space

---
--- selectors for numbers, comments, strings and bools
---
numberSelector :: Parser ByteString Node
numberSelector = fmap Number C.scientific

multilineCommentSelector :: Parser ByteString Node
multilineCommentSelector =
  C.string "/*" *> C.manyTill C.anyChar (C.string "*/") <&> parseComment
  where
    parseComment = MultilineComment . T.strip . T.pack

singlelineCommentSelector :: Parser ByteString Node
singlelineCommentSelector =
  C.string "//" *> C.takeWhile ('\n' /=) <&> parseComment
  where
    parseComment = SinglelineComment . T.strip . decodeUtf8

commentSelector :: Parser ByteString Node
commentSelector = multilineCommentSelector <|> singlelineCommentSelector

nullSelector :: Parser ByteString Node
nullSelector = C.string "null" $> Null

boolSelector :: Parser ByteString Node
boolSelector =
  C.choice [C.string "true", C.string "false"] <&> Bool . (== "true")

stringSelector :: Parser ByteString Node
stringSelector =
  (C.char '"' *> C.takeWhile ('"' /=) <* C.char '"') <&> String . decodeUtf8

scalarSelector :: Parser ByteString Node
scalarSelector =
  stringSelector
    <|> commentSelector
    <|> numberSelector
    <|> boolSelector
    <|> nullSelector

nodeSelector :: Parser ByteString Node
nodeSelector = skipWhiteSpace *> anyNode
  where
    anyNode = scalarSelector <|> arraySelector <|> objectSelector

---
--- selectors for objects, object keys and arrays
---
arrayElemSelector :: Parser ByteString Node
arrayElemSelector = do
  n <- nodeSelector
  c <- C.peekChar
  case c of
    Just ']' -> return n
    _ -> separatorSelector $> n

arraySelector :: Parser ByteString Node
arraySelector = do
  _ <- C.char '['
  elems <- C.many' arrayElemSelector
  _ <- C.char ']'
  return . Array . V.fromList $ elems

objectKeySelector :: Parser ByteString Node
objectKeySelector = do
  _ <- skipWhiteSpace
  key <- stringSelector
  _ <- skipWhiteSpace
  _ <- C.char ':'
  value <- nodeSelector
  let obj = ObjectKey (key, value)
  c <- C.peekChar
  case c of
    Just '}' -> return obj
    _ -> separatorSelector $> obj

objectSelector :: Parser ByteString Node
objectSelector = do
  _ <- C.char '{'
  keys <- C.many' (commentSelector <|> objectKeySelector)
  _ <- C.char '}'
  return . Object . V.fromList $ keys

parseNodes :: ByteString -> Either String Node
parseNodes =
  C.parseOnly
    $ nodeSelector <* skipWhiteSpace <* (C.endOfInput <?> "unexpected input")
