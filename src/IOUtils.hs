module IOUtils (tryReadFile, putErrorLine, reportInvalidNodes) where

import Control.Exception (IOException, try)
import Core.Node (Node)
import Data.ByteString.Lazy qualified as BL (
  ByteString,
 )
import Data.Text qualified as T (append)
import Formatting (formatNode, newRuleSet)
import GHC.IO.Exception (IOErrorType, IOException (IOError))
import System.IO (hPutStrLn)

putErrorLine :: Text -> IO ()
putErrorLine = hPutStrLn stderr . toString

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
    then Left $ maybe "" appendColon filename `T.append` toText ioe_desc
    else Right ""
  where
    appendColon f = toText f `T.append` ": "
ioErrorMsg _ (Right valid) = Right valid

tryReadFile :: [IOErrorType] -> FilePath -> IO (Either Text BL.ByteString)
tryReadFile noerrs fp = do
  possiblyContent <-
    try (readFileLBS fp)
      :: IO (Either IOException BL.ByteString)
  pure $ ioErrorMsg noerrs possiblyContent
