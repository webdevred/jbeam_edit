module IOUtils (tryReadFile) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import GHC.IO.Exception (IOErrorType, IOException (IOError))

import Data.ByteString.Lazy qualified as BL (
  ByteString,
  readFile,
 )
import Data.Text qualified as T (append, pack)

ioErrorMsg
  :: [IOErrorType]
  -> Either Control.Exception.IOException BL.ByteString
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
    Control.Exception.try (BL.readFile fp)
      :: IO (Either Control.Exception.IOException BL.ByteString)
  pure $ ioErrorMsg noerrs possiblyContent
