module JbeamEdit.IOUtils (tryReadFile, putErrorLine, putErrorStringLn, reportInvalidNodes) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL (
  ByteString,
 )
import Data.Text (Text)
import Data.Text qualified as T (append, pack, unpack)
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
    then Left $ maybe "" appendColon filename `T.append` T.pack ioe_desc
    else Right ""
  where
    appendColon f = T.pack f `T.append` ": "
ioErrorMsg _ (Right valid) = Right valid

tryReadFile :: [IOErrorType] -> OsPath -> IO (Either Text BL.ByteString)
tryReadFile noerrs fp = ioErrorMsg noerrs <$> try (OS.readFile fp)
