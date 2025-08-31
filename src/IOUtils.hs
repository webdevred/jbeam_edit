module IOUtils (tryReadFile) where

import Control.Exception (IOException, try)
import GHC.IO.Exception (IOErrorType, IOException (IOError))

import Data.ByteString.Lazy qualified as BL (
  ByteString,
 )
import Data.Text qualified as T (append)

ioErrorMsg
  :: [IOErrorType]
  -> Either Control.Exception.IOException BL.ByteString
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
    Control.Exception.try (readFileLBS fp)
      :: IO (Either Control.Exception.IOException BL.ByteString)
  pure $ ioErrorMsg noerrs possiblyContent
