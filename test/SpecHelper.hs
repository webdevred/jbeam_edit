module SpecHelper (
  textToLazyByteString,
  applySpecOnInput,
  works,
  listFilesInDir,
  module JbeamEdit.Core.Node,
  module Test.Hspec,
  DescribeFun,
  SpecFun,
) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS (fromStrict)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import JbeamEdit.Core.Node
import System.Directory (getDirectoryContents)
import Test.Hspec

type DescribeFun = (String -> String -> String)

type SpecFun t1 t2 a = (t1 -> t2 -> a)

listFilesInDir
  :: FilePath
  -> IO [String]
listFilesInDir dir =
  filter (\f -> isSuffixOf ".hs" f && not (".#" `isPrefixOf` f))
    <$> getDirectoryContents dir

applySpecOnInput
  :: (Example a, Show t1, Show t2)
  => DescribeFun
  -> SpecFun t1 t2 a
  -> t1
  -> t2
  -> SpecWith (Arg a)
applySpecOnInput descFun spec input expResult =
  describe (descFun (show input) (show expResult)) . works $
    spec input expResult

works :: Example a => a -> SpecWith (Arg a)
works = it "works"

textToLazyByteString :: String -> ByteString
textToLazyByteString = BS.fromStrict . encodeUtf8 . T.pack
