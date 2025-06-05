module Parsing.Jbeam (
  nodeParser,
  numberParser,
  parseNodes,
) where

import Control.Applicative (Alternative (..), optional, (<|>))
import Core.Node (InternalComment (..), Node (..))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Text (Text)
import Parsing.Common
import Text.Megaparsec ((<?>))

import Data.Text qualified as T
import Data.Vector qualified as V (fromList)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (scientific)
import Text.Megaparsec.Char qualified as C

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
numberParser = do
  char <- MP.lookAhead B.asciiChar
  Number
    <$> if char == toWord8 '-'
      then
        negate <$> (byteChar '-' *> L.scientific)
      else
        L.scientific

multilineCommentParser :: Parser InternalComment
multilineCommentParser =
  C.string "/*" >> parseComment (MP.manyTill B.asciiChar (B.string "*/"))
  where
    multilineComment text = InternalComment {cText = text, cMultiline = True}
    parseComment = parseWord8s (multilineComment . T.strip)

singlelineCommentParser :: Parser InternalComment
singlelineCommentParser =
  C.string "//" *> parseComment (MP.some (MP.satisfy (charNotEqWord8 '\n')))
  where
    singlelineComment text = InternalComment {cText = text, cMultiline = False}
    parseComment = parseWord8s (singlelineComment . T.strip)

commentParser :: Parser Node
commentParser =
  MP.label
    "comment"
    (Comment <$> tryParsers [multilineCommentParser, singlelineCommentParser])

nullParser :: Parser Node
nullParser = C.string "null" $> Null

boolParser :: Parser Node
boolParser = Bool <$> parseBool

stringParser :: Parser Node
stringParser = parseWord8s String string
  where
    validString =
      byteChar '"' *> MP.some (MP.satisfy (charNotEqWord8 '"')) <* byteChar '"'
    emptyString = C.string "\"\"" >> pure []
    string = emptyString <|> validString

scalarParser :: Parser Node
scalarParser =
  tryScalarParsers
    [stringParser, commentParser, numberParser, boolParser, nullParser]
  where
    tryScalarParsers = MP.try . tryParsers . map MP.hidden

nodeParser :: Parser Node
nodeParser = skipWhiteSpace *> (anyNode <|> failingParser expLabels)
  where
    expLabels = ["a valid scalar", "object", "array"]
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

parseNodes :: ByteString -> Either Text Node
parseNodes = first formatErrors . MP.parse topNodeParser "<input>"
