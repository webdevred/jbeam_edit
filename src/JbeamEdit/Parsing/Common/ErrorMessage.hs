module JbeamEdit.Parsing.Common.ErrorMessage (
  formatErrors,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Function (on)
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word8)
import JbeamEdit.Parsing.Common.Helpers (charNotEqWord8, toChar, toWord8)
import Text.Megaparsec qualified as MP

joinAndFormatToks :: Set (MP.ErrorItem Word8) -> Text
joinAndFormatToks = T.concat . reverse . f [] . S.elems
  where
    f acc toks =
      case toks of
        [tok] -> pure (formatTok tok)
        [tok, tok2] -> formatTok tok2 : " or " : formatTok tok : acc
        (tok : toks') -> f (", " : formatTok tok : acc) toks'
        [] -> acc

formatTok :: MP.ErrorItem Word8 -> Text
formatTok toks =
  case toks of
    MP.EndOfInput -> "end of input"
    MP.Label lab -> T.pack $ NE.toList lab
    MP.Tokens toks' -> T.pack . wrap "'" "'" . map toChar $ NE.toList toks'

errorAreaAndLineNumber :: Int -> LBS.ByteString -> (Text, Text)
errorAreaAndLineNumber pos inputNotParsed =
  let (begin, end) = LBS.splitAt (fromIntegral pos) inputNotParsed
      fstPartOfLine = LBS.takeWhileEnd (charNotEqWord8 '\n') begin
      sndPartOfLine = LBS.takeWhile (charNotEqWord8 '\n') end
      lineNumber = LBS.count (toWord8 '\n') begin
      errorArea =
        T.strip $
          on (<>) (decodeUtf8Lenient . LBS.toStrict) fstPartOfLine sndPartOfLine
   in (errorArea, T.show lineNumber)

wrap :: Semigroup a => a -> a -> a -> a
wrap l r m = l <> m <> r

formatTrivialErrors
  :: Int
  -> LBS.ByteString
  -> Set (MP.ErrorItem Word8)
  -> Maybe (MP.ErrorItem Word8)
  -> Text
formatTrivialErrors pos inputNotParsed expToks unexpTok =
  let formattedUnexpTok = maybe "" (wrap "got: " ", " . formatTok) unexpTok
      (errorArea, lineNumber) = errorAreaAndLineNumber pos inputNotParsed
   in formattedUnexpTok
        <> "expecting "
        <> joinAndFormatToks expToks
        <> " somewhere close to "
        <> errorArea
        <> " on line "
        <> lineNumber

formatFancyErrors
  :: Int
  -> LBS.ByteString
  -> Set (MP.ErrorFancy e)
  -> Text
formatFancyErrors = error "unreachable, we dont do fancy errors so far"

formatErrors :: MP.ParseErrorBundle LBS.ByteString e -> Text
formatErrors bundle =
  let MP.ParseErrorBundle
        { MP.bundleErrors = bunErrs
        , MP.bundlePosState = posState
        } = bundle
      MP.PosState {MP.pstateInput = inputNotParsed} = posState
      formatBundleError err =
        case err of
          (MP.TrivialError pos unexpToks expToks) ->
            formatTrivialErrors pos inputNotParsed expToks unexpToks
          (MP.FancyError pos err') -> formatFancyErrors pos inputNotParsed err'
   in T.intercalate "\n" . map formatBundleError . NE.toList $ bunErrs
