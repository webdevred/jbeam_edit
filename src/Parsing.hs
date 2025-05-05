module Parsing
  ( parseNodes
  ) where

import Control.Applicative ((<|>), asum, optional)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr, ord)
import Data.Function (on)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty qualified as LV
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V (fromList)
import Data.Void (Void)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (lexeme, scientific, signed)
import Text.Megaparsec.Char qualified as C

import Data.Set (Set)
import Data.Set qualified as S

import Data.Text (Text)
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

joinAndFormatToks :: Set (MP.ErrorItem Word8) -> Text
joinAndFormatToks = T.concat . reverse . f [] . S.elems
  where
    f acc toks =
      case toks of
        [tok] -> pure (formatTok tok)
        [tok, tok2] -> formatTok tok2 : " or " : formatTok tok : acc
        (tok:toks') -> f (", " : formatTok tok : acc) toks'
        [] -> acc

formatTok :: MP.ErrorItem Word8 -> Text
formatTok toks =
  case toks of
    MP.EndOfInput -> "end of input"
    MP.Label lab -> T.pack $ LV.toList lab
    MP.Tokens toks' -> T.pack . wrap "'" "'" . map toChar $ LV.toList toks'

errorArea :: Int -> ByteString -> Text
errorArea pos inputNotParsed =
  let (begin, end) = BS.splitAt pos inputNotParsed
      fstPartOfLine = BS.takeWhileEnd ((/=) '\n' . toChar) begin
      sndPartOfLine = BS.takeWhile ((/=) '\n' . toChar) end
   in T.strip $ on (<>) decodeUtf8 fstPartOfLine sndPartOfLine

wrap :: Semigroup a => a -> a -> a -> a
wrap l r m = l <> m <> r

formatTrivialErrors ::
     Int
  -> ByteString
  -> Set (MP.ErrorItem Word8)
  -> Maybe (MP.ErrorItem Word8)
  -> Text
formatTrivialErrors pos inputNotParsed expToks unexpToks =
  let formattedUnexpTok = maybe "" (wrap "got: " ", " . formatTok) unexpToks
   in formattedUnexpTok
        <> "expecting "
        <> joinAndFormatToks expToks
        <> " somewhere close to "
        <> errorArea pos inputNotParsed

formatFancyErrors = undefined

formatErrors :: MP.ParseErrorBundle ByteString Void -> Text
formatErrors bundle =
  let MP.ParseErrorBundle { MP.bundleErrors = bunErrs
                          , MP.bundlePosState = posState
                          } = bundle
      MP.PosState {MP.pstateInput = inputNotParsed} = posState
      formatBundleError err =
        case err of
          (MP.TrivialError pos unexpToks expToks) ->
            formatTrivialErrors pos inputNotParsed expToks unexpToks
          (MP.FancyError pos err) -> formatFancyErrors pos inputNotParsed err
   in T.intercalate "\n" . map formatBundleError . LV.toList $ bunErrs

parseNodes :: ByteString -> Either Text Node
parseNodes = first formatErrors . MP.parse topNodeParser "<input>"
  where
    topNodeParser = nodeParser <* skipWhiteSpace <* MP.eof
