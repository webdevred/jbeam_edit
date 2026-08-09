module JbeamEdit.Core.Newline (detectNewline) where

import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8 (elemIndex)
import System.IO (Newline (..))

detectNewline :: LazyByteString -> Newline
detectNewline content =
  case LBS8.elemIndex '\r' content of
    Nothing -> LF
    Just idx ->
      case LBS.uncons (LBS.drop (idx + 1) content) of
        Just (10, _) -> CRLF -- '\n'
        _ -> detectNewline (LBS.drop (idx + 1) content)
