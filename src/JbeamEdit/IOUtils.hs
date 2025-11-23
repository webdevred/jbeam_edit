module JbeamEdit.IOUtils (tryReadFile, putErrorLine, putErrorStringLn, reportInvalidNodes) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL (
  ByteString,
 )
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T (append, pack, unpack)
import GHC.IO.Exception (IOErrorType, IOException (IOError))
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Formatting (formatNode, newRuleSet)
import System.IO (hPutStrLn, stderr)

putErrorStringLn :: String -> IO ()
putErrorStringLn = hPutStrLn stderr

putErrorLine :: Text -> IO ()
putErrorLine = putErrorStringLn . T.unpack

reportInvalidNodes :: Text -> [Node] -> IO ()
reportInvalidNodes msg nodes =
  unless (null nodes) $ do
    putErrorLine msg
    mapM_ (putErrorLine . formatNode newRuleSet) nodes

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

tryReadFile :: [IOErrorType] -> FilePath -> IO (Either Text BL.ByteString)
tryReadFile noerrs fp = do
  possiblyContent <-
    try (LBS.readFile fp)
      :: IO (Either IOException BL.ByteString)
  pure $ ioErrorMsg noerrs possiblyContent
