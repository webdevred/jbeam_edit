module IOUtils (tryReadFile, putErrorLine, reportInvalidNodes) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Core.Node (Node)
import Data.ByteString.Lazy qualified as BL (
  ByteString,
 )
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T (append, pack, unpack)
import Formatting (formatNode, newRuleSet)
import GHC.IO.Exception (IOErrorType, IOException (IOError))
import System.IO (hPutStrLn, stderr)

putErrorLine :: Text -> IO ()
putErrorLine = hPutStrLn stderr . T.unpack

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
