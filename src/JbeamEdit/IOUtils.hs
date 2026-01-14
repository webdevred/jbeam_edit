module JbeamEdit.IOUtils (
  isNotEmacsBackupFile,
  pathEndsWithExtension,
  tryReadFile,
  putErrorLine,
  putErrorStringLn,
  humanJoin,
  intToText,
  reportInvalidNodes,
) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL (
  ByteString,
 )
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T (append, pack, unpack)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder qualified as TLB (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import GHC.IO.Exception (IOErrorType, IOException (IOError))
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Formatting (formatNode)
import System.File.OsPath qualified as OS
import System.IO (hPutStrLn, stderr)
import System.OsPath

putErrorStringLn :: String -> IO ()
putErrorStringLn = hPutStrLn stderr

putErrorLine :: Text -> IO ()
putErrorLine = putErrorStringLn . T.unpack

reportInvalidNodes :: Text -> [Node] -> IO ()
reportInvalidNodes msg nodes =
  unless (null nodes) $
    putErrorLine msg
      >> mapM_ (putErrorLine . formatNode mempty) nodes

ioErrorMsg
  :: [IOErrorType]
  -> Either IOException BL.ByteString
  -> Either Text BL.ByteString
ioErrorMsg noerrs (Left (IOError _ ioe_type _ ioe_desc _ filename)) =
  if ioe_type `notElem` noerrs
    then Left $ foldMap appendColon filename `T.append` T.pack ioe_desc
    else Right ""
  where
    appendColon f = T.pack f `T.append` ": "
ioErrorMsg _ (Right valid) = Right valid

tryReadFile :: [IOErrorType] -> OsPath -> IO (Either Text BL.ByteString)
tryReadFile noerrs fp = ioErrorMsg noerrs <$> try (OS.readFile fp)

isNotEmacsBackupFile :: OsPath -> Bool
isNotEmacsBackupFile = not . all (isPrefixOf ".#") . decodeBaseFileName
  where
    decodeBaseFileName :: OsPath -> Maybe FilePath
    decodeBaseFileName = decodeUtf . takeFileName

pathEndsWithExtension :: String -> OsPath -> Bool
pathEndsWithExtension expectedExt filepath =
  let (_, ext) = splitExtensions filepath
   in unsafeEncodeUtf expectedExt == ext

humanJoin :: Foldable t => Text -> t Text -> Text
humanJoin lastCombiner =
  TL.toStrict . B.toLazyText . go mempty . toList
  where
    go acc [] = acc
    go acc [x] = acc <> B.fromText x
    go acc [x, y] =
      acc
        <> B.fromText x
        <> " "
        <> B.fromText lastCombiner
        <> " "
        <> B.fromText y
    go acc (x : xs) =
      go (acc <> B.fromText x <> B.fromText ", ") xs

intToText :: Integral a => a -> Text
intToText = TL.toStrict . TLB.toLazyText . decimal
