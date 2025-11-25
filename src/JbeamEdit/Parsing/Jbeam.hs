module JbeamEdit.Parsing.Jbeam (
  ParseState (..),
  JbeamParser,
  nodeParser,
  numberParser,
  parseNodes,
  parseNodesState,
) where

import Control.Monad.State (State, evalState)
import Control.Monad.State.Class
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Vector qualified as V (fromList)
import Data.Void (Void)
import JbeamEdit.Core.Node (
  AssociationDirection (..),
  InternalComment (..),
  Node (..),
 )
import JbeamEdit.Parsing.Common
import Text.Megaparsec ((<?>), (<|>))
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
  ws1 <- MP.takeWhileP Nothing wordIsSpace
  comma <- MP.optional (MP.label "comma" $ byteChar ',')
  ws2 <- MP.takeWhileP Nothing wordIsSpace

  let hasNewline =
        isNothing comma && '\n' `elem` map toChar (LBS.unpack ws1)
          || '\n' `elem` map toChar (LBS.unpack ws2)

  modify (\s -> s {lastNodeEndedWithNewline = hasNewline})

  pure ()

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

associationDirection :: ParseState -> AssociationDirection
associationDirection st =
  if not (lastNodeEndedWithNewline st)
    then
      PreviousNode
    else
      NextNode

commentStripSpace :: Text -> Text
commentStripSpace initialText =
  let initialNewline = bool "" "\n" (T.isPrefixOf "\n" initialText)
      trimTrailingSpaces = T.dropWhileEnd (charBoth (/= '\n') isSpace)
      endingNewline = bool "" "\n" (T.isSuffixOf "\n" $ trimTrailingSpaces initialText)
      go = T.intercalate "\n" . filter (not . T.all isSpace) . map T.strip . T.lines
   in initialNewline <> go initialText <> endingNewline

multilineCommentParser :: JbeamParser InternalComment
multilineCommentParser = do
  st <- get
  let multilineComment text =
        InternalComment
          { cText = text
          , cMultiline = True
          , cAssociationDirection = associationDirection st
          }
      parseComment = parseWord8s (multilineComment . commentStripSpace)
  C.string "/*" >> parseComment (MP.manyTill B.asciiChar (B.string "*/"))

singlelineCommentParser :: JbeamParser InternalComment
singlelineCommentParser = do
  st <- get
  _ <- C.string "//"
  txt <- MP.many (MP.satisfy (charNotEqWord8 '\n'))
  pure
    InternalComment
      { cText = T.strip . decodeUtf8Lenient $ BS.pack txt
      , cMultiline = False
      , cAssociationDirection = associationDirection st
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
  _ <- MP.optional separatorParser
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
  skipWhiteSpace
  keys <- MP.many (commentParser <* skipWhiteSpace <|> objectKeyParser)
  _ <- MP.optional separatorParser
  _ <- byteChar '}'
  pure . Object . V.fromList $ keys

topNodeParser :: JbeamParser Node
topNodeParser = nodeParser <* skipWhiteSpace <* MP.eof

parseNodesState
  :: JbeamParser a
  -> LBS.ByteString
  -> Either (MP.ParseErrorBundle LBS.ByteString Void) a
parseNodesState parser input =
  let initialState = ParseState {lastNodeEndedWithNewline = True}
   in evalState (MP.runParserT parser "<input>" input) initialState

parseNodes :: LBS.ByteString -> Either Text Node
parseNodes input = first formatErrors (parseNodesState topNodeParser input)
