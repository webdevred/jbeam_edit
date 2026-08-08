module JbeamEdit.Core.Newline (detectNewline) where

import Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 as LBS8 (elemIndex)
import System.IO (Newline (..))

detectNewline :: LBS.ByteString -> Newline
detectNewline content =
  case LBS8.elemIndex '\r' content of
    Nothing -> LF
    Just idx ->
      case LBS.index content (idx + 1) of
        10 -> CRLF -- '\n'
        _ -> detectNewline (LBS.drop (idx + 1) content)
