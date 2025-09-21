module Parsing.Jbeam (
  nodeParser,
  numberParser,
  parseNodes,
) where

import Core.Node (InternalComment (..), Node (..))
import Parsing.Common
import Text.Megaparsec ((<?>))

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V (fromList)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (scientific)
import Text.Megaparsec.Char qualified as C

newtype ParseState = ParseState
  { lastNodeEndedWithNewline :: Bool
  }

type JbeamParser a = Parser (State ParseState) a

separatorParser :: JbeamParser ()
separatorParser = do
  ws1 <-
    MP.takeWhileP
      (Just "space before comma")
      wordIsSpace
  comma <- optional (byteChar ',')
  ws2 <-
    MP.takeWhileP
      (Just "space after comma")
      wordIsSpace

  let hasNewline =
        isNothing comma && '\n' `elem` map toChar (BS.unpack ws1)
          || '\n' `elem` map toChar (BS.unpack ws2)

  modify (\s -> s {lastNodeEndedWithNewline = hasNewline})

  pass

---
--- selectors for numbers, comments, strings and bools
---
numberParser :: JbeamParser Node
numberParser = do
  char <- MP.lookAhead B.asciiChar
  Number
    <$> if char == toWord8 '-'
      then
        negate <$> (byteChar '-' *> L.scientific)
      else
        L.scientific

multilineCommentParser :: JbeamParser InternalComment
multilineCommentParser = do
  st <- get
  let multilineComment text =
        InternalComment
          { cText = text
          , cMultiline = True
          , assocWithPrior = not (lastNodeEndedWithNewline st)
          }
      parseComment = parseWord8s (multilineComment . T.strip)
  C.string "/*" >> parseComment (MP.manyTill B.asciiChar (B.string "*/"))

singlelineCommentParser :: JbeamParser InternalComment
singlelineCommentParser = do
  st <- get
  _ <- C.string "//"
  txt <- MP.some (MP.satisfy (charNotEqWord8 '\n'))
  pure
    InternalComment
      { cText = T.strip (decodeUtf8 (BS.pack txt))
      , cMultiline = False
      , assocWithPrior = not (lastNodeEndedWithNewline st)
      }

commentParser :: JbeamParser Node
commentParser =
  MP.label
    "comment"
    (Comment <$> tryParsers [multilineCommentParser, singlelineCommentParser])

nullParser :: JbeamParser Node
nullParser = C.string "null" $> Null

boolParser :: JbeamParser Node
boolParser = Bool <$> parseBool

stringParser :: JbeamParser Node
stringParser = parseWord8s String string
  where
    validString =
      byteChar '"' *> MP.some (MP.satisfy (charNotEqWord8 '"')) <* byteChar '"'
    emptyString = C.string "\"\"" >> pure []
    string = emptyString <|> validString

scalarParser :: JbeamParser Node
scalarParser =
  tryScalarParsers
    [stringParser, commentParser, numberParser, boolParser, nullParser]
  where
    tryScalarParsers = MP.try . tryParsers . map MP.hidden

nodeParser :: JbeamParser Node
nodeParser = skipWhiteSpace *> (anyNode <|> failingParser expLabels)
  where
    expLabels = ["a valid scalar", "object", "array"]
    anyNode = MP.try (tryParsers [arrayParser, objectParser, scalarParser])

---
--- selectors for objects, object keys and arrays
---
arrayParser :: JbeamParser Node
arrayParser = do
  _ <- byteChar '['
  elems <- MP.sepEndBy nodeParser separatorParser
  _ <- optional separatorParser
  _ <- byteChar ']'
  pure . Array . V.fromList $ elems

objectKeyParser :: JbeamParser Node
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

objectParser :: JbeamParser Node
objectParser = do
  _ <- byteChar '{'
  keys <- MP.some (commentParser <|> objectKeyParser)
  _ <- optional separatorParser
  _ <- byteChar '}'
  pure . Object . V.fromList $ keys

topNodeParser :: JbeamParser Node
topNodeParser = nodeParser <* skipWhiteSpace <* MP.eof

parseNodes :: BS.ByteString -> Either Text Node
parseNodes input =
  let initialState = ParseState {lastNodeEndedWithNewline = False}
      result = evalState (MP.runParserT topNodeParser "<input>" input) initialState
   in first formatErrors result
